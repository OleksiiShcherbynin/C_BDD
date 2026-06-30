#include "bdd.c"
#include <time.h>

#define TESTS_PER_N    100
#define MAX_EXPR_LEN   8192

/* ГЕНЕРАЦИЯ СЛУЧАЙНЫХ ВЫРАЖЕНИЙ */

void gen_random_expr(char *buffer, int *pos, int num_vars, int depth) {
    if (*pos > MAX_EXPR_LEN - 32) { //ограничения на длинну
        buffer[(*pos)++] = 'A' + rand() % num_vars;
        return;
    }

    /*  переменная или константа */
    if (depth <= 0 || (depth < 3 && rand() % 3 == 0)) {
        if (rand() % 12 == 0)
            buffer[(*pos)++] = '0' + rand() % 2;
        else
            buffer[(*pos)++] = 'A' + rand() % num_vars;
        return;
    }

    int choice = rand() % 10;

    if (choice < 2) {
        /* NOT */
        buffer[(*pos)++] = '!';
        gen_random_expr(buffer, pos, num_vars, depth - 1);
    } else if (choice < 6) {
        /* AND*/
        buffer[(*pos)++] = '(';
        gen_random_expr(buffer, pos, num_vars, depth - 1);
        gen_random_expr(buffer, pos, num_vars, depth - 1);
        buffer[(*pos)++] = ')';
    } else {
        /* OR */
        buffer[(*pos)++] = '(';
        gen_random_expr(buffer, pos, num_vars, depth - 1);
        buffer[(*pos)++] = '+';
        gen_random_expr(buffer, pos, num_vars, depth - 1);
        buffer[(*pos)++] = ')';
    }
}


int verify_bdd(const char *expr, BDD *bdd) {
    ASTNode *ast = parse_expression(expr, bdd->var_list, bdd->num_vars);
    if (!ast) return -1;

    int error_count = 0;
    int total_combos = 1 << bdd->num_vars;

    for (int combo = 0; combo < total_combos; combo++) {
        char inputs[27];
        int  values[26] = {0};

        for (int i = 0; i < bdd->num_vars; i++) {
            int bit = (combo >> (bdd->num_vars - 1 - i)) & 1;
            inputs[i] = '0' + bit;
            values[i] = bit;
        }
        inputs[bdd->num_vars] = '\0';

        char bdd_result = BDD_use(bdd, inputs);
        int  expected   = evaluate_ast(ast, values);
        char expected_ch = expected ? '1' : '0';

        if (bdd_result != expected_ch) {
            error_count++;
            if (error_count <= 3)
                printf("    input=%s  BDD=%c  expected=%c\n",
                       inputs, bdd_result, expected_ch);
        }
    }

    free_ast(ast);
    return error_count;
}

/* Выборочная верификация для больших N и проверяет sample_count случайных комбинаций */
int verify_bdd_sampled(const char *expr, BDD *bdd, int sample_count) {
    ASTNode *ast = parse_expression(expr, bdd->var_list, bdd->num_vars);
    if (!ast) return -1;

    int error_count = 0;

    for (int s = 0; s < sample_count; s++) {
        char inputs[27];
        int  values[26] = {0};

        for (int i = 0; i < bdd->num_vars; i++) {
            int bit = rand() & 1;
            inputs[i] = '0' + bit;
            values[i] = bit;
        }
        inputs[bdd->num_vars] = '\0';

        char bdd_result = BDD_use(bdd, inputs);
        int  expected   = evaluate_ast(ast, values);
        char expected_ch = expected ? '1' : '0';

        if (bdd_result != expected_ch) {
            error_count++;
            if (error_count <= 3)
                printf("    input=%s  BDD=%c  expected=%c\n",
                       inputs, bdd_result, expected_ch);
        }
    }

    free_ast(ast);
    return error_count;
}

/* РУЧНЫЕ ТЕСТЫ */

typedef struct {
    const char *expression;
    const char *description;
} ManualTest;

void run_manual_tests(void) {
    ManualTest tests[] = {
        { "AB+C","(A AND B) OR C - example from assignment" },
        { "AB","A AND B" },
        { "A+B","A OR B" },
        { "!A","NOT A" },
        { "!(A+B)*C","NOT(A OR B) AND C" },
        { "A*B+C*D","(A AND B) OR (C AND D)" },
        { "A+!A","tautology (always 1)" },
        { "A*!A","contradiction (always 0)" },
        { "A*(B+C)","A AND (B OR C)" },
        { "!(!A*!B)","NOT(NOT A AND NOT B) = A OR B (De Morgan)" },
        { "(A+B)*(C+D)*(E+!E)","five variables" },
    };
    int num_tests = sizeof(tests) / sizeof(tests[0]);

    printf("MANUAL TESTS (%d cases)\n", num_tests);

    int total_pass = 0;

    for (int t = 0; t < num_tests; t++) {
        const char *expr = tests[t].expression;

        char var_list[27];
        int num_vars;
        extract_variables(expr, var_list, &num_vars);

        BDD *bdd = BDD_create((char *)expr, var_list);
        if (!bdd) {
            printf("FAIL \"%s\" - failed to create BDD\n", expr);
            continue;
        }

        int errors = verify_bdd(expr, bdd);
        long long full_size = (1LL << (num_vars + 1)) - 1;
        double reduction = (1.0 - (double)bdd->node_count / full_size) * 100.0;

        if (errors == 0) {
            printf("PASS %-25s  nodes: %-5d  reduction: %5.1f%%   %s\n",
                   expr, bdd->node_count, reduction, tests[t].description);
            total_pass++;
        } else {
            printf("FAIL %-25s  errors: %d\n", expr, errors);
        }

        BDD_free(bdd);
    }

    printf("\n  Total: %d/%d passed\n", total_pass, num_tests);
}

/*  АВТОМ ТЕСТЫ  */

void run_auto_tests(int num_vars, int test_count) {
    printf("--- N = %d, %d functions ---\n", num_vars, test_count);

    char order[27];
    for (int i = 0; i < num_vars; i++)
        order[i] = 'A' + i;
    order[num_vars] = '\0';

    long long full_bdd_size = (1LL << (num_vars + 1)) - 1;

    /* для N > 14 используем выборочную верификацию */
    int use_sampling = (num_vars > 14);
    int sample_count = 10000;

    int total_errors = 0;
    double sum_reduction = 0.0;
    double sum_best_reduc = 0.0;
    double sum_create_time = 0.0;
    double sum_best_time = 0.0;
    double sum_extra_reduc = 0.0;

    int depth = 3;
    if (num_vars > 5)  depth = 4;
    if (num_vars > 9)  depth = 5;
    if (num_vars > 12) depth = 6;
    if (num_vars > 16) depth = 7;

    for (int t = 0; t < test_count; t++) {
        char expr_buffer[MAX_EXPR_LEN];
        int  pos = 0;
        gen_random_expr(expr_buffer, &pos, num_vars, depth);
        expr_buffer[pos] = '\0';

        // printf(" %s\n", expr_buffer);

        /* BDD_create */
        clock_t time_start = clock();
        BDD *bdd = BDD_create(expr_buffer, order);
        clock_t time_end = clock();

        double create_ms = (double)(time_end - time_start) / CLOCKS_PER_SEC * 1000.0;
        sum_create_time += create_ms;

        if (!bdd) { total_errors++; continue; }

        int errors = use_sampling
            ? verify_bdd_sampled(expr_buffer, bdd, sample_count)
            : verify_bdd(expr_buffer, bdd);
        total_errors += errors;

        double reduction = (1.0 - (double)bdd->node_count / full_bdd_size) * 100.0;
        sum_reduction += reduction;
        int create_nodes = bdd->node_count;
        BDD_free(bdd);

        /* BDD_create_with_best_order*/
        time_start = clock();
        BDD *best_bdd = BDD_create_with_best_order(expr_buffer);
        time_end = clock();

        double best_ms = (double)(time_end - time_start) / CLOCKS_PER_SEC * 1000.0;
        sum_best_time += best_ms;

        if (best_bdd) {
            double best_reduc = (1.0 - (double)best_bdd->node_count / full_bdd_size) * 100.0;
            sum_best_reduc += best_reduc;

            if (create_nodes > 0) {
                double extra = (1.0 - (double)best_bdd->node_count / create_nodes) * 100.0;
                sum_extra_reduc += extra;
            }
            BDD_free(best_bdd);
        }
    }

    printf("average %% reduction (BDD_create):               %6.5f%%\n",
           sum_reduction / test_count);
    printf("average %% reduction (best_order):                %6.5f%%\n",
           sum_best_reduc / test_count);
    printf("average %% extra reduction (best vs create):       %6.5f%%\n",
           sum_extra_reduc / test_count);
    printf("average time BDD_create:                       %8.3f ms\n",
           sum_create_time / test_count);
    printf("average time BDD_create_with_best_order:       %8.3f ms\n",
           sum_best_time / test_count);
    if (use_sampling)
        printf("verification: %d samples/test (sampled)\n", sample_count);
    else
        printf("  Verification: all 2^%d = %.0f combos (exhaustive)\n",
               num_vars, (double)(1LL << num_vars));
    printf("errors: %d / %d\n\n", total_errors, test_count);
}

/* MAIN */

int main(void) {
    srand((unsigned int)time(NULL));

    printf("TESTING BDD \n");

    run_manual_tests();

    printf("AUTOMATIC TESTS\n");

    int test_sizes[]  = {13, 15, 17, 19, 21};
    int test_counts[] = {100,100, 50, 30, 20};
    int num_sizes = sizeof(test_sizes) / sizeof(test_sizes[0]);

    for (int i = 0; i < num_sizes; i++) {
        run_auto_tests(test_sizes[i], test_counts[i]);
    }

    printf("Testing complete.\n");



    return 0;
}
