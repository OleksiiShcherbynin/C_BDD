/*
 *  Парсер булевых выражений (AST) + построение BDD с онлайн-редукцией.

 *  Операторы: !, *, +
 *  Приоритет: NOT > AND > OR
 *  Переменные: A-Z
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Абстрактное синтаксическое дерево */

typedef enum {
    AST_VAR,
    AST_CONST,
    AST_NOT,
    AST_AND,
    AST_OR
} ASTType;

typedef struct ASTNode {
    ASTType type;
    int var_index;
    int const_value;
    struct ASTNode *left;
    struct ASTNode *right;
} ASTNode;

ASTNode* create_var_node(int var_index) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    node->type = AST_VAR;
    node->var_index = var_index;
    node->const_value = 0;
    node->left = NULL;
    node->right = NULL;
    return node;
}

ASTNode* create_const_node(int value) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    node->type = AST_CONST;
    node->var_index = -1;
    node->const_value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}

ASTNode* create_unary_node(ASTType type, ASTNode *operand) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    node->type = type;
    node->var_index = -1;
    node->const_value = 0;
    node->left = operand;
    node->right = NULL;
    return node;
}

ASTNode* create_binary_node(ASTType type, ASTNode *left, ASTNode *right) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    node->type = type;
    node->var_index = -1;
    node->const_value = 0;
    node->left = left;
    node->right = right;
    return node;
}

void free_ast(ASTNode *node) {
    if (!node) return;
    free_ast(node->left);
    free_ast(node->right);
    free(node);
}

int evaluate_ast(ASTNode *node, int *var_values) {
    if (!node) return 0;
    switch (node->type) {
        case AST_VAR:   return var_values[node->var_index];
        case AST_CONST: return node->const_value;
        case AST_NOT:   return !evaluate_ast(node->left, var_values);
        case AST_AND:   return evaluate_ast(node->left, var_values) && evaluate_ast(node->right, var_values);
        case AST_OR:    return evaluate_ast(node->left, var_values) || evaluate_ast(node->right, var_values);
        default:        return 0;
    }
}

/* ТОКЕНИЗАТОР */

typedef enum {
    TOK_VAR, TOK_CONST, TOK_NOT, TOK_AND, TOK_OR,
    TOK_LPAREN, TOK_RPAREN, TOK_END
} TokenType;

typedef struct {
    TokenType type;
    int value;
} Token;

Token* tokenize(const char *expr, const char *var_list, int num_vars, int *out_token_count) {
    int capacity = 128;
    int count = 0;
    Token *tokens = (Token *)malloc(capacity * sizeof(Token));

    for (int i = 0; expr[i]; i++) {
        if (count + 2 >= capacity) {
            capacity *= 2;
            tokens = (Token *)realloc(tokens, capacity * sizeof(Token));
        }

        char ch = expr[i];

        if (ch == ' ' || ch == '\t') continue;

        if (ch >= 'a' && ch <= 'z') ch = ch - 'a' + 'A';

        if (ch >= 'A' && ch <= 'Z') {
            int var_idx = -1;
            for (int j = 0; j < num_vars; j++) {
                if (var_list[j] == ch) { var_idx = j; break; }
            }
            if (var_idx < 0) {
                fprintf(stderr, "Error: unknown variable '%c'\n", ch);
                free(tokens);
                *out_token_count = 0;
                return NULL;
            }
            tokens[count].type  = TOK_VAR;
            tokens[count].value = var_idx;
            count++;
        } else if (ch == '0' || ch == '1') {
            tokens[count].type  = TOK_CONST;
            tokens[count].value = ch - '0';
            count++;
        } else if (ch == '!') { tokens[count].type = TOK_NOT;    tokens[count].value = 0; count++; }
          else if (ch == '*') { tokens[count].type = TOK_AND;    tokens[count].value = 0; count++; }
          else if (ch == '+') { tokens[count].type = TOK_OR;     tokens[count].value = 0; count++; }
          else if (ch == '(') { tokens[count].type = TOK_LPAREN; tokens[count].value = 0; count++; }
          else if (ch == ')') { tokens[count].type = TOK_RPAREN; tokens[count].value = 0; count++; }
    }

    tokens[count].type  = TOK_END;
    tokens[count].value = 0;
    count++;

    *out_token_count = count;
    return tokens;
}

/* ПАРСЕР
 *  NOT > AND > OR

 *  or_expr   -> and_expr ('+' and_expr)*
 *  and_expr  -> not_expr (('*'?) not_expr)*
 *  not_expr  -> '!' not_expr | primary
 *  primary   -> '(' or_expr ')'
 */

typedef struct {
    Token *token_list;
    int position;
    int count;
} Parser;
// вернуть то, что сейчас парсируем
Token parser_current(Parser *parser) {
    return parser->token_list[parser->position];
}
//сдвинуть позицию
void parser_advance(Parser *parser) {
    if (parser->position < parser->count - 1)
        parser->position++;
}
//чтоб найти неявное *
int is_term_start(TokenType type) {
    return type == TOK_VAR || type == TOK_CONST ||
           type == TOK_NOT || type == TOK_LPAREN;
}

ASTNode* parse_or_expr(Parser *parser);
ASTNode* parse_and_expr(Parser *parser);
ASTNode* parse_not_expr(Parser *parser);
ASTNode* parse_primary(Parser *parser);

ASTNode* parse_or_expr(Parser *parser) {
    ASTNode *left = parse_and_expr(parser);
    while (parser_current(parser).type == TOK_OR) {
        parser_advance(parser);
        ASTNode *right = parse_and_expr(parser);
        left = create_binary_node(AST_OR, left, right);
    }
    return left;
}

ASTNode* parse_and_expr(Parser *parser) {
    ASTNode *left = parse_not_expr(parser);
    while (parser_current(parser).type == TOK_AND || is_term_start(parser_current(parser).type)) {
        if (parser_current(parser).type == TOK_AND)
            parser_advance(parser);
        ASTNode *right = parse_not_expr(parser);
        left = create_binary_node(AST_AND, left, right);
    }
    return left;
}

ASTNode* parse_not_expr(Parser *parser) {
    if (parser_current(parser).type == TOK_NOT) {
        parser_advance(parser);
        ASTNode *operand = parse_not_expr(parser);
        return create_unary_node(AST_NOT, operand);
    }
    return parse_primary(parser);
}

ASTNode* parse_primary(Parser *parser) {
    Token tok = parser_current(parser);

    if (tok.type == TOK_LPAREN) {
        parser_advance(parser);
        ASTNode *expr = parse_or_expr(parser);
        if (parser_current(parser).type == TOK_RPAREN)
            parser_advance(parser);
        return expr;
    }
    if (tok.type == TOK_VAR) {
        parser_advance(parser);
        return create_var_node(tok.value);
    }
    if (tok.type == TOK_CONST) {
        parser_advance(parser);
        return create_const_node(tok.value);
    }

    fprintf(stderr, "unexpected token\n");
    return create_const_node(0);
}

ASTNode* parse_expression(const char *expr, const char *var_list, int num_vars) {
    int token_count;
    Token *tokens = tokenize(expr, var_list, num_vars, &token_count);
    if (!tokens) return create_const_node(0);

    Parser parser;
    parser.token_list = tokens;
    parser.position = 0;
    parser.count = token_count;

    ASTNode *ast = parse_or_expr(&parser);
    free(tokens);
    return ast;
}

/* СТРУКТУРЫ BDD */

typedef struct BDD_Node {
    int var_index; /* индекс переменной \*/
    int value; /* значение терминала */
    struct BDD_Node *low; /* 0 */
    struct BDD_Node *high; /* 1 */
} BDD_Node;


typedef struct UniqueEntry {
    int var_index;
    BDD_Node *low;
    BDD_Node *high;
    BDD_Node *node;
    struct UniqueEntry *next; // бакет
} UniqueEntry;

#define UNIQUE_TABLE_SIZE 100003

typedef struct {
    UniqueEntry **buckets;
} UniqueTable;

/* основна */

typedef struct BDD {
    int num_vars;
    int node_count;
    BDD_Node *root;
    char var_list[27]; /* отсорт список */
    BDD_Node **node_pool;/* выделенные узлы */
    int pool_size;
    int pool_capacity;
    BDD_Node *terminal_zero;
    BDD_Node *terminal_one;
} BDD;

/* уник табл */

UniqueTable* create_unique_table(void) {
    UniqueTable *table = (UniqueTable *)malloc(sizeof(UniqueTable));
    table->buckets = (UniqueEntry **)calloc(UNIQUE_TABLE_SIZE, sizeof(UniqueEntry *));
    return table;
}

unsigned int compute_hash(int var_index, BDD_Node *low, BDD_Node *high) {
    unsigned long hash_val = (unsigned long)var_index;
    hash_val = hash_val * 1000003UL + (unsigned long)(uintptr_t)low;
    hash_val = hash_val * 1000003UL + (unsigned long)(uintptr_t)high;
    return (unsigned int)(hash_val % UNIQUE_TABLE_SIZE);
}
// ищем для будущ вставке
BDD_Node* unique_search(UniqueTable *table, int var_index, BDD_Node *low, BDD_Node *high) {
    unsigned int hash_val = compute_hash(var_index, low, high);
    UniqueEntry *entry = table->buckets[hash_val];
    while (entry) {
        if (entry->var_index == var_index && entry->low == low && entry->high == high)
            return entry->node;
        entry = entry->next;
    }
    return NULL;
}
// есди не нашли узел в сёрч, идём сюда
void unique_insert(UniqueTable *table, int var_index, BDD_Node *low, BDD_Node *high, BDD_Node *node) {
    unsigned int hash_val = compute_hash(var_index, low, high);
    UniqueEntry *entry = (UniqueEntry *)malloc(sizeof(UniqueEntry));
    entry->var_index = var_index;
    entry->low  = low;
    entry->high = high;
    entry->node = node;
    entry->next = table->buckets[hash_val];
    table->buckets[hash_val] = entry;
}

void free_unique_table(UniqueTable *table) {
    for (int i = 0; i < UNIQUE_TABLE_SIZE; i++) {
        UniqueEntry *entry = table->buckets[i];
        while (entry) {
            UniqueEntry *next_entry = entry->next;
            free(entry);
            entry = next_entry;
        }
    }
    free(table->buckets);
    free(table);
}

/* УПРАВЛЕНИЕ УЗЛАМИ БДД */

BDD_Node* allocate_bdd_node(BDD *bdd) {
    if (bdd->pool_size >= bdd->pool_capacity) {
        bdd->pool_capacity *= 2;
        bdd->node_pool = (BDD_Node **)realloc(bdd->node_pool, bdd->pool_capacity * sizeof(BDD_Node *));
    }
    BDD_Node *node = (BDD_Node *)malloc(sizeof(BDD_Node));
    bdd->node_pool[bdd->pool_size++] = node;
    return node;
}
//создание узла - листа
BDD_Node* create_terminal_node(BDD *bdd, int value) {
    BDD_Node *node = allocate_bdd_node(bdd);
    node->var_index = -1;
    node->value = value;
    node->low = NULL;
    node->high = NULL;
    return node;
}
// создание узла - развилки -- внутринние узлы
BDD_Node* create_decision_node(BDD *bdd, int var_index, BDD_Node *low, BDD_Node *high) {
    BDD_Node *node = allocate_bdd_node(bdd);
    node->var_index = var_index;
    node->value = -1;
    node->low = low;
    node->high = high;
    return node;
}

/* ПОСТРОЕНИЕ БДД */

BDD_Node* build_recursive(ASTNode *ast, int *var_values, int level, int num_vars, int *var_order,
                                                                                UniqueTable *unique_table, BDD *bdd) {
    if (level == num_vars) { // глубина равна количеству переменных
        int result = evaluate_ast(ast, var_values);
        return result ? bdd->terminal_one : bdd->terminal_zero;
    }
    // берём след перменную из порядка
    int current_var = var_order[level];
    // лево 0
    var_values[current_var] = 0;
    BDD_Node *low_child = build_recursive(ast, var_values, level + 1, num_vars, var_order, unique_table, bdd);
    // право 1
    var_values[current_var] = 1;
    BDD_Node *high_child = build_recursive(ast, var_values, level + 1, num_vars, var_order, unique_table, bdd);

    var_values[current_var] = -1;

    /* первая проверка редукции если дети ведут в одно место */
    if (low_child == high_child)
        return low_child;

    /* проверка существует ли уже узел + работа с хэшом */
    BDD_Node *existing = unique_search(unique_table, current_var, low_child, high_child);
    if (existing)
        return existing;
    // создаём узел / кешируем
    BDD_Node *new_node = create_decision_node(bdd, current_var, low_child, high_child);
    unique_insert(unique_table, current_var, low_child, high_child, new_node);
    return new_node;
}

/*  */
// извлечение переменных
void extract_variables(const char *expr, char *var_list, int *out_num_vars) {
    int found[26] = {0}; //англ алфавит, 26 нулей
    for (int i = 0; expr[i]; i++) {
        char ch = expr[i];
        if (ch >= 'a' && ch <= 'z') ch = ch - 'a' + 'A';
        if (ch >= 'A' && ch <= 'Z')
            found[ch-'A'] = 1;
    }
    *out_num_vars = 0;
    for (int i = 0; i < 26; i++) {
        if (found[i])
            var_list[(*out_num_vars)++] = 'A' + i;
    }
    var_list[*out_num_vars] = '\0';
}

/* ПУБЛИЧНЫЙ API */
void BDD_free(BDD *bdd) {
    if (!bdd) return;
    for (int i = 0; i < bdd->pool_size; i++)
        free(bdd->node_pool[i]);
    free(bdd->node_pool);
    free(bdd);
}

char BDD_use(BDD *bdd, char *vstupy) {
    if (!bdd || !bdd->root || !vstupy) return -1;
    if ((int)strlen(vstupy) != bdd->num_vars) return -1;

    for (int i = 0; i < bdd->num_vars; i++) {
        if (vstupy[i] != '0' && vstupy[i] != '1') return -1;
    }

    BDD_Node *current_node = bdd->root;
    while (current_node->var_index != -1) {
        int input_val = vstupy[current_node->var_index] - '0';
        current_node = input_val ? current_node->high : current_node->low;
    }

    return current_node->value ? '1' : '0';
}

BDD* BDD_create(char *bfunkcia, char *poradie) {
    if (!bfunkcia || !poradie) return NULL;

    int num_vars = (int)strlen(poradie);
    if (num_vars == 0 || num_vars > 26) return NULL;

    /* отсорт список переменных */
    char sorted_vars[27];
    memcpy(sorted_vars, poradie, num_vars);
    sorted_vars[num_vars] = '\0';

    for (int i = 0; i < num_vars - 1; i++) {
        for (int j = i + 1; j < num_vars; j++) {
            if (sorted_vars[i] > sorted_vars[j]) {
                char temp = sorted_vars[i];
                sorted_vars[i] = sorted_vars[j];
                sorted_vars[j] = temp;
            }
        }
    }

    /* Порядок переменных - poradie[level] → индекс в sorted_vars */
    int var_order[26];
    for (int level = 0; level < num_vars; level++) {
        for (int idx = 0; idx < num_vars; idx++) {
            if (sorted_vars[idx] == poradie[level]) {
                var_order[level] = idx;
                break;
            }
        }
    }

    ASTNode *ast = parse_expression(bfunkcia, sorted_vars, num_vars);
    if (!ast) return NULL;

    BDD *bdd = (BDD *)malloc(sizeof(BDD));
    bdd->num_vars = num_vars;
    bdd->node_count = 0;
    bdd->pool_size = 0;
    bdd->pool_capacity = 1024;
    bdd->node_pool = (BDD_Node **)malloc(bdd->pool_capacity * sizeof(BDD_Node *));
    strcpy(bdd->var_list, sorted_vars);

    bdd->terminal_zero = create_terminal_node(bdd, 0);
    bdd->terminal_one = create_terminal_node(bdd, 1);

    int var_values[26];
    for (int i = 0; i < 26; i++) var_values[i] = -1;

    UniqueTable *unique_table = create_unique_table();// готовим память для хэш таблицы

    bdd->root = build_recursive(ast, var_values, 0, num_vars, var_order, unique_table, bdd);//делаем

    /* подсчёт узлов*/
    if (bdd->root->var_index == -1)
        bdd->node_count = 1;
    else
        bdd->node_count = bdd->pool_size;

    free_unique_table(unique_table);
    free_ast(ast);
    return bdd;
}

BDD* BDD_create_with_best_order(char *bfunkcia) {
    if (!bfunkcia) return NULL;

    char var_list[27];
    int num_vars;
    extract_variables(bfunkcia, var_list, &num_vars);
    if (num_vars == 0) return NULL;

    BDD *best_bdd = NULL;
    int  best_count = 0x7FFFFFFF; // худшее количество узлов

    /* Циклические сдвиги порядка(ABC, BCA) num_vars попыток */
    for (int shift = 0; shift < num_vars; shift++) {
        char order[27];
        for (int i = 0; i < num_vars; i++)
            order[i] = var_list[(i + shift) % num_vars];
        order[num_vars] = '\0';

        BDD *bdd = BDD_create(bfunkcia, order);
        if (!bdd) continue;

        if (bdd->node_count < best_count) {
            if (best_bdd) BDD_free(best_bdd);
            best_bdd   = bdd;
            best_count = bdd->node_count;
        } else {
            BDD_free(bdd);
        }
    }

    /* Дополнительные случайные перестановки (ещё N штук) */
    for (int trial = 0; trial < num_vars; trial++) {
        char order[27];
        memcpy(order, var_list, num_vars);
        order[num_vars] = '\0';

        /* Фишер-Йетс перемешивание */
        for (int i = num_vars - 1; i > 0; i--) {
            int j = rand() % (i + 1);
            char temp = order[i];
            order[i] = order[j];
            order[j] = temp;
        }

        BDD *bdd = BDD_create(bfunkcia, order);
        if (!bdd) continue;

        if (bdd->node_count < best_count) {
            if (best_bdd) BDD_free(best_bdd);
            best_bdd   = bdd;
            best_count = bdd->node_count;
        } else {
            BDD_free(bdd);
        }
    }

    return best_bdd;
}
/* ДОП ФУНКЦИИ */

int BDD_node_count(BDD *bdd) {
    return bdd ? bdd->node_count : 0;
}

int BDD_var_count(BDD *bdd) {
    return bdd ? bdd->num_vars : 0;
}

// тест через АСТ дерево
int evaluate_with_varlist(const char *expr, const char *var_list,
                          int num_vars, const char *inputs) {
    int var_values[26] = {0};
    for (int i = 0; i < num_vars && inputs[i]; i++)
        var_values[i] = inputs[i] - '0'; // делаем из строк массив

    ASTNode *ast = parse_expression(expr, var_list, num_vars);
    int result = evaluate_ast(ast, var_values);
    free_ast(ast);
    return result;
}
