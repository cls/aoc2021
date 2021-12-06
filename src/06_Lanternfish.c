#include <stdio.h>

enum { Ages = 9 };

int
main(int argc, char **argv)
{
    long fish[Ages] = {0};

    int c;

    while ((c = getchar()) != EOF) {
        if (c >= '0' && c < '0' + Ages) {
            fish[c-'0']++;
        }
    }

    int day = 0;

    for (; day < 80; day++) {
        fish[(day + 7) % Ages] += fish[day % Ages];
    }

    long day80 = 0;

    for (int age = 0; age < Ages; age++) {
        day80 += fish[age];
    }

    printf("%ld\n", day80);

    for (; day < 256; day++) {
        fish[(day + 7) % Ages] += fish[day % Ages];
    }

    long day256 = 0;

    for (int age = 0; age < Ages; age++) {
        day256 += fish[age];
    }

    printf("%ld\n", day256);

    return 0;
}
