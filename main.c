#include <stdio.h>
#include <stdlib.h>
#include <math.h>




void up_triangle(double **a, int n, int m, int *cnt)
{
    int ind;
    *cnt = 0;
    double tmp, mult;
    for (int i = 0; i < n - 1; ++i) {
        // Меняем местами i-ую строку с той,
        //у который ненулевой элемент i-го столбца
        ind = i;
        while (a[ind][i] == 0 && ind < n) {
            ++ind; //Так как задана невырожденная матрица,
            // такой индекс найдется
        }
        if (ind != i) {
            *cnt = (*cnt + i - ind) % 2;
            for (int j = 0; j < m; ++j) {
                tmp = a[ind][j];
                a[ind][j] = a[i][j];
                a[i][j] = tmp;
            }
        }
        //Вычитаем из всех последующих строк i-ую,
        // умноженную на a[k][i]/a[i][i]
        for(int k = i + 1; k < n; ++k) {
            mult = a[k][i] / a[i][i];
            for (int j = i; j < m; ++j) {
                a[k][j] -= a[i][j] * mult;
            }
        }
    }
    return;
}

void up_triangle_modified(double **a, int n, int m, int *cnt, int *permutations)
{
    int ind_c, ind_r, itmp;
    *cnt = 0;
    double tmp, mult;
    for (int i = 0; i < n - 1; ++i) {
        /* Ищем максимальный по модулю элемент и
            меняем местами i-ую строку и столбец с теми, где расположен
                максимальный элемент */
        ind_c = i;
        ind_r = i;
        double maximum = a[i][i];
        for (int k = i; k < n; ++k){
            for (int j = i; j < n; ++j) {
                if (fabs(a[k][j]) - fabs(maximum) > 0) {
                    ind_c = j;
                    ind_r = k;
                    maximum = a[k][j];
                }
            }
        }
        *cnt = (*cnt + 2 * i - ind_c - ind_r) % 2;
        //Запоминаем, какие столбцы менялись местами
        itmp = permutations[i];
        permutations[i] = permutations[ind_c];
        permutations[ind_c] = itmp;
        for (int j = 0; j < m; ++j) {
            tmp = a[ind_r][j];
            a[ind_r][j] = a[i][j];
            a[i][j] = tmp;
        }
        for (int k = 0; k < n; ++k) {
            tmp = a[k][ind_c];
            a[k][ind_c] = a[k][i];
            a[k][i] = tmp;
        }
        for(int k = i + 1; k < n; ++k) {
            mult = a[k][i] / a[i][i];
            for (int j = i; j < m; ++j) {
                a[k][j] -= a[i][j] * mult;
            }
        }
    }
}

void normalize(double **a, int n, int m)
{
    for (int i = 0; i < n; ++i) {
        double d = a[i][i];
        for (int j = 0; j < m; ++j) {
            a[i][j] /= d;
        }
    }
    return;
}

void diagonal(double **a, int n, int m)
{
    for (int i = n - 1; i > 0; --i) {
        for (int k = 0; k < i; ++k) {
            double mult = a[k][i] / a[i][i];
            for (int j = 0; j < m; ++j) {
                a[k][j] -= a[i][j] * mult;
            }
        }
    }
    return;
}

double det_count(double **a, int n)
{
    int cnt;
    up_triangle(a, n, n, &cnt);
    double det = 1;
    for (int i = 0; i < n; ++i) {
        det *= a[i][i];
    }
    if (cnt % 2) {
        det *= -1;
    }
    return det;
}

double **m_allocate(int n, int m)
{
    double **a;
    a = calloc(n, sizeof(a[0]));

    for (int i = 0; i < n; ++i) {
        a[i] = calloc(m, sizeof(a[0][0]));
    }
    return a;
}

double *iterations_mod(double **a, int n, double *f, double eps, double w)
{
    double *x_prev = calloc(n, sizeof(x_prev[0]));

    double *x_next = calloc(n, sizeof(x_next[0]));
    for (int i = 0; i < n; ++i) {
        x_prev[i] = 0;
        x_next[i] = 0;
    }
    double p = 1;
    double sum1, sum2;
    int count = 0;
    while (p > eps) {
        p = 0;
        ++count;
        //printf("%d ", ++k);
        //if (k > 5) {
            //break;
        //}
        for (int i = 0; i < n; ++i) {
            sum1 = 0;
            sum2 = 0;
            for(int j = 0; j < i; ++j) {
                sum1 -= a[i][j] * x_next[j] * w / a[i][i];
            }
            for(int j = i + 1; j < n; ++j) {
                sum2 -= a[i][j] * x_prev[j] * w / a[i][i];
            }
            x_next[i] = sum1 + sum2 + f[i] * w / a[i][i] - x_prev[i] * (w - 1);
            p += fabs(x_next[i] - x_prev[i]);
            //printf("%lf ", x_next[i]);
        }
        //printf("\n");
        for (int i = 0; i < n; ++i) {
            x_prev[i] = x_next[i];
        }
        //printf("p = %lf\n", p);
    }
    printf("Count of iterations: %d\n", count);
    return x_next;
}

double *iterations(double **a, int n, double *f, double eps, double w)
{
    double *x_prev = calloc(n, sizeof(x_prev[0]));

    double *x_next = calloc(n, sizeof(x_next[0]));
    for (int i = 0; i < n; ++i) {
        x_prev[i] = 0;
        x_next[i] = 0;
    }
    double p = 1;
    double sum1, sum2;
    int count = 0;
    while (p > eps) {
        p = 0;
        ++count;
        for (int i = 0; i < n; ++i) {
            sum1 = 0;
            sum2 = 0;
            for(int j = 0; j < i; ++j) {
                sum1 += a[i][j] * x_next[j];
            }
            for(int j = i; j < n; ++j) {
                sum2 += a[i][j] * x_prev[j];
            }
            x_next[i] = x_prev[i] + (f[i] - sum1 - sum2) * w / a[i][i];
            p += fabs(x_next[i] - x_prev[i]);
        }
        for (int i = 0; i < n; ++i) {
            x_prev[i] = x_next[i];
        }
    }
    printf("Count of iterations: %d\n", count);
    return x_next;
}

double norm(double **a, int n)
{
    double sum, m = 0;
    for (int i = 0; i < n; ++i) {
        sum = 0;
        for (int j = 0; j < n; ++j) {
            sum += fabs(a[i][j]);
        }
        if (sum > m) {
            m = sum;
        }
    }
    return m;
}

int main(void)
{
    int i, j, n, flag;
    printf("Put the value of n\n");
    scanf("%d", &n);
    // Выделяем память под матрицу А, вектор-функцию и расширенную матрицу
    double **a, **b;
    a = m_allocate(n, n);
    b = m_allocate(n, n + 1);
    double *f = calloc(n, sizeof(f[0]));


    printf("Set the format of matrix initialization (0 - keyboard input, 1 - formula)\n");
    scanf("%d", &flag);
    if (flag) {
        int m = 20;
        for (i = 0; i < n; ++i) {
            for (j = 0; j < n; ++j) {
                if (i != j) {
                    a[i][j] = ((double)(i + j)) / ((double)(m + n));
                } else {
                    a[i][j] = n + m * m +((double)i) / ((double)n) +((double)j) / ((double)m);
                }
                b[i][j] = a[i][j];

            }
            printf("\n");
        }
        for (i = 0; i < n; ++i) {
            b[i][n] = m * i + n;
            f[i] = b[i][n];
        }
    } else {
        for (i = 0; i < n; ++i) {
            for (j = 0; j < n; ++j) {
                scanf("%lf", &a[i][j]);
                b[i][j] = a[i][j];
            }
        }
        for (i = 0; i < n; ++i) {
            scanf("%lf", &b[i][n]);
            f[i] = b[i][n];
        }
    }
    for (i = 0; i < n; ++i) {
        for (j = n; j < n; ++j){
            printf("%lf ", a[i][j]);
        }
        printf("\n");
    }
    //Вычисление определителя
    printf("det(A) = %lf\n", det_count(a, n));

    //Вычисление обратной матрицы
    double **c = m_allocate(n, 2 * n);
    for (i = 0; i < n; ++i) {
        for (j = 0; j < n; ++j) {
            c[i][j] = b[i][j];
        }
        for (j = 0; j < n; ++j) {
            if (i == j) {
                c[i][j + n] = 1;
            } else {
                c[i][j + n] = 0;
            }
        }
    }
    int cnt;
    up_triangle(c, n, 2 * n, &cnt);
    diagonal(c, n, 2 * n);
    normalize(c, n, 2 * n);
    printf("Reversed matrix:\n");
    for (i = 0; i < n; ++i) {
        for (j = n; j < 2 * n; ++j){
            printf("%lf ", c[i][j]);
            c[i][j - n] = c[i][j];
        }
        printf("\n");
    }
    double m = norm(b, n) * norm(c, n);
    printf("M = %lf\n", m);

    printf("Set the method \n(0 - Gauss, \n1 - Gauss modified, \n2 - relaxation)\n");
    scanf("%d", &flag);
    printf("The solution:\n");
    int ind;
    if (flag == 0) {
        up_triangle(b, n, n + 1, &cnt);
        diagonal(b, n, n + 1);
        normalize(b, n, n + 1);
        for (i = 0; i < n; ++i) {
            printf("%lf\n", b[i][n]);
        }
    } else if (flag == 1) {
        int *permutations;
        permutations = calloc(n, sizeof(int));
        for (i = 0; i < n; ++i) {
            permutations[i] = i;
        }
        up_triangle_modified(b, n, n + 1, &cnt, permutations);
        diagonal(b, n, n + 1);
        normalize(b, n, n + 1);
        for (i = 0; i < n; ++i) {
            ind = 0;
            while (permutations[ind] != i) {
                ++ind;
            }
            printf("%lf\\ ", b[ind][n]);
        }
    } else {
        double eps = 0.00001;
        double w = 1.5;
        double *solution = calloc(n, sizeof(solution[0]));
        solution = iterations(b, n, f, eps, w);
        for (i = 0; i < n; ++i) {
            printf("%lf\\ ", solution[i]);
        }
    }


    return 0;
}
