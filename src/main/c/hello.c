#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>


#define TUTORIAL_PART 7

char fakeInput[] = "asdf ghjkl!!\nanother line\nohh!!";
int fakeInputPosition = 0;
// fake function to avoid real input
char getchar_fake() {
	if (fakeInputPosition == sizeof(fakeInput)) return EOF;
	else return fakeInput[fakeInputPosition++];
}

#define with_args(values...) values
#define format_as(resultVar, format, formatArgs) char *resultVar = (char *) malloc(10000); sprintf(resultVar, format, formatArgs);

void stringShouldEqual(char expected[], char actual[]) {
	int result = strcmp(expected, actual);
	if (result == 0) return;

	fprintf(stderr, "Expected:\n'%s'\nbut was\n'%s'\n", expected, actual);
	assert(0);
}

void intsShouldEqual(int expected, int actual) {
	if (expected == actual) return;
	fprintf(stderr, "Expected: '%d' but was '%d'\n", expected, actual);
	assert(0);
}

int my_power(int base, int n) {
	int i, p;
	p = 1;
	for (i = 1; i <= n; ++i) p = p * base;
	return p;
}

void part_1_5_2() {
    long nc = 0;
//    while (getchar_fake() != EOF) ++nc;
    for (nc = 0; getchar_fake() != EOF; ++nc);

    format_as(actual, "%ld\n", nc);
    stringShouldEqual("32\n", actual);
}

void part_1_5_3() {
    int c, nl;
    nl = 0;
    while ((c = getchar_fake()) != EOF) {
        if (c == '\n') ++nl;
    }

    format_as(actual, "%d\n", nl);
    stringShouldEqual("2\n", actual);
}

void part_1_5_4() {
    #define IN   1  /* inside a word */
    #define OUT  0  /* outside a word */

	int c, nl, nw, nc, state;
    state = OUT;
    nl = nw = nc = 0;
    while ((c = getchar_fake()) != EOF) {
        ++nc;
        if (c == '\n')
            ++nl;
        if (c == ' ' || c == '\n' || c == '\t')
            state = OUT;
        else if (state == OUT) {
            state = IN;
			++nw;
		}
	}

	format_as(actual, "%d %d %d\n", with_args(nl, nw, nc));
	stringShouldEqual("2 5 32\n", actual);
}


int main() {
    if (TUTORIAL_PART == 0) {
        printf("hello, !!!! ooooa aaa ");
        printf("world");
        printf("\n");

    } else if (TUTORIAL_PART == 1) {
	    #define LOWER  0
	    #define UPPER  300
	    #define STEP   20

        int fahr = LOWER;
        while (fahr <= UPPER) {
            int celsius = 5 * (fahr-32) / 9;
            printf("%d\t%d\n", fahr, celsius);
            fahr = fahr + STEP;
        }

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
            printf("%d %d %d\n", i, my_power(2,i), my_power(-3,i));

    } else if (TUTORIAL_PART == 4) {
        char c;
        while ((c = getchar_fake()) != EOF) putchar(c);

    } else if (TUTORIAL_PART == 5) {
        part_1_5_2();

    } else if (TUTORIAL_PART == 6) {
        part_1_5_3();

    } else if (TUTORIAL_PART == 7) {
        part_1_5_4();
    }

}