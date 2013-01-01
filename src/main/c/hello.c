#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>


#define with_args(values...) values
#define format_as(resultVar, format, formatArgs) char *resultVar = (char *) malloc(10000); sprintf(resultVar, format, formatArgs);

void stringShouldEqual(const char expected[], const char actual[]) {
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

void part_1_1() {
	printf("hello, !!!! ooooa aaa ");
	printf("world");
	printf("\n");
}

void part_1_2() {
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
}


char* fakeInput = "asdf ghjkl!!\nanother line\nohh!!";
int fakeInputSize = 0;
int fakeInputPosition = 0;
// fake function to avoid real input
char getchar_fake() {
	if (fakeInputPosition == strlen(fakeInput)) return EOF;
	else return fakeInput[fakeInputPosition++];
}

void init_fake_input_to(char s[]) {
	fakeInput = s;
	fakeInputSize = strlen(s);
	fakeInputPosition = 0;
}


void part_1_5_1() {
	fakeInputPosition = 0;

    char c;
    while ((c = getchar_fake()) != EOF) putchar(c);
}

void part_1_5_2() {
	init_fake_input_to("asdf ghjkl!!\nanother line\nohh!!");

    long nc = 0;
//    while (getchar_fake() != EOF) ++nc;
    for (nc = 0; getchar_fake() != EOF; ++nc);

    format_as(actual, "%ld\n", nc);
    stringShouldEqual("31\n", actual);
}

void part_1_5_3() {
	fakeInputPosition = 0;

    int c, nl;
    nl = 0;
    while ((c = getchar_fake()) != EOF) {
        if (c == '\n') ++nl;
    }

    format_as(actual, "%d\n", nl);
    stringShouldEqual("2\n", actual);
}

void part_1_5_4() {
	fakeInputPosition = 0;

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
	stringShouldEqual("2 5 31\n", actual);
}

void part_1_6() {
    int ndigit[10];
    ndigit[0] = 12;
    ndigit[1] = 14;

    int i;
    for (i = 0; i < 10; ++i) {
        printf(" %d", ndigit[i]);
    }
}

void part_1_7() {
	int i;
	for (i = 0; i < 10; ++i)
	    printf("%d %d %d\n", i, my_power(2,i), my_power(-3,i));
}

/* getline:  read a line into s, return length  */
int getline_1_8(char s[],int lim) {
	int c, i;
	for (i=0; i < lim-1 && (c=getchar_fake())!=EOF && c!='\n'; ++i)
		s[i] = c;
	if (c == '\n') {
        s[i] = c;
		++i;
	}
	s[i] = '\0';
	return i;
}

/* copy:  copy ÕfromÕ into ÕtoÕ; assume to is big enough */
void copy(char to[], char from[]) {
	int i;
    i = 0;
    while ((to[i] = from[i]) != '\0') ++i;
}

void part_1_8() {
	#define MAXLINE 1000   /* maximum input line length */

	fakeInputPosition = 0;

	int len; /* longest line saved here */
	int max; /* current line length */
	char line[MAXLINE]; /* maximum length seen so far */
	char longest[MAXLINE]; /* current input line */

	max = 0;
	while ((len = getline_1_8(line, MAXLINE)) > 0)
	   if (len > max) {
	       max = len;
	       copy(longest, line);
	   }
	if (max > 0) /* there was a line */
	   printf("%s", longest);
}

void part_2_2() {
    short int shortInt;
    long int longInt;
    // short float short Float;  // error
    // long float longFloat;     // error
    // short double shortDouble; // error
    long double longDouble;

    signed char signedChar;
    unsigned char unsignedChar;
    signed int signedInt;
    unsigned int unsignedInt;
    // signed double signedDouble; // error
    // unsigned double unsignedDouble; // error
}

void part_2_3() {
    #define A_LONG 12345L
    #define AN_UNSIGNED_LONG 12345UL

    enum months { JAN = 1, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC };
    printf("%d, %d, %d", JAN, FEB, MAR);
}

int my_atoi(char s[]) {
	int i, n;
	n = 0;
	for (i = 0; s[i] >= '0' && s[i] <= '9'; ++i)
	   n = 10 * n + (s[i] - '0');
	return n;
}

int my_lower(int c) {
	if (c >= 'A' && c <= 'Z')
	   return c + 'a' - 'A';
	else
	   return c;
}

void part_2_7() {
	printf("%d\n", my_atoi("12345"));
	printf("%d\n", my_atoi("12345qwerty"));
	printf("%c", my_lower('P'));
	printf("%c", my_lower('I'));
	printf("%c", my_lower('g'));
}

#define MAXOP   100  /* max size of operand or operator */
#define NUMBER  '0'  /* signal that a number was found */

#define MAXVAL 100  /* maximum depth of val stack */
int sp = 0;         /* next free stack position */
double val[MAXVAL]; /* value stack */

/* push:  push f onto value stack */
void push(double f) {
	if (sp < MAXVAL)
		val[sp++] = f;
	else
		printf("error: stack full, canÕt push %g\n", f);
}

/* pop:  pop and return top value from stack */
double pop(void) {
	if (sp > 0) {
		return val[--sp];
	} else {
		printf("error: stack empty\n");
		return 0.0;
	}
}

#define BUFSIZE 100
char buf[BUFSIZE];    /* buffer for ungetch */
int bufp = 0;         /* next free position in buf */

/* get a (possibly pushed-back) character */
int getch(void) {
	return (bufp > 0) ? buf[--bufp] : getchar_fake();
}

/* push character back on input */
void ungetch(int c) {
	if (bufp >= BUFSIZE)
		printf("ungetch: too many characters\n");
	else
		buf[bufp++] = c;
}

int getop(char s[]) {
	int i, c;
	while ((s[0] = c = getch()) == ' ' || c == '\t');

	s[1] = '\0';
	if (!isdigit(c) && c != '.') return c; /* not a number */

	i = 0;
	if (isdigit(c)) /* collect integer part */
		while (isdigit(s[++i] = c = getch()));

	if (c == '.') /* collect fraction part */
		while (isdigit(s[++i] = c = getch()));

	s[i] = '\0';
	if (c != EOF) ungetch(c);
	return NUMBER;
}

void part_4_3() {
    init_fake_input_to("2 2 3 + *\n");

	int type;
	double op2;
	char s[MAXOP];

	while ((type = getop(s)) != EOF) {
		printf("%s\n", s);

		switch (type) {
			case NUMBER:
				push(atof(s));
				break;
			case '+':
				push(pop() + pop());
				break;
			case '*':
				push(pop() * pop());
				break;
			case '-':
				op2 = pop();
				push(pop() - op2);
				break;
			case '/':
				op2 = pop();
				if (op2 != 0.0)
				   push(pop() / op2);
				else
				   printf("error: zero divisor\n");
				break;
			case '\n':
				printf("\t%.8g\n", pop());
				break;
			default:
				printf("error: unknown command %s\n", s);
			break;
		}
	}
}

int main() {
    part_1_1();
    part_1_2();
    part_1_5_1();
    part_1_5_2();
    part_1_5_3();
    part_1_5_4();
    part_1_6();
    part_1_7();
    part_1_8();

    part_2_2();
    part_2_3();
    part_2_7();

    part_4_3();
}