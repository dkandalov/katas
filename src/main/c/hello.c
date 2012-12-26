#include <stdio.h>

#define LOWER  0
#define UPPER  300
#define STEP   20

#define TUTORIAL_PART 3

int main() {
    if (TUTORIAL_PART == 0) {
        printf("hello, !!!! ooooa aaa ");
        printf("world");
        printf("\n");

    } else if (TUTORIAL_PART == 1) {
        int fahr = LOWER;
        while (fahr <= UPPER) {
            int celsius = 5 * (fahr-32) / 9;
            printf("%d\t%d\n", fahr, celsius);
            fahr = fahr + STEP;
        }

        // int fahr;
        for (fahr = LOWER; fahr <= UPPER; fahr = fahr + STEP)
        printf("%3d %6.1f\n", fahr, (5.0/9.0)*(fahr-32));

    } else if (TUTORIAL_PART == 2) {
        int ndigit[10];
        ndigit[0] = 12;
        ndigit[1] = 14;

        int i;
        for (i = 0; i < 10; ++i) {
            printf(" %d", ndigit[i]);
        }

    } else if (TUTORIAL_PART == 3) {
        int i;
    	for (i = 0; i < 10; ++i)
            printf("%d %d %d\n", i, power(2,i), power(-3,i));
    }
}

int power(int base, int n) {
	int i, p;
	p = 1;
	for (i = 1; i <= n; ++i) p = p * base;
	return p;
}