#include <stdio.h>

static int step_octopuses(char *octopuses);
static void step_octopus(size_t i, char *octopuses);

enum { Flash = 10, Width = 10, Height = 10, Size = Width * Height };

int
main(int argc, char **argv)
{
    char octopuses[Size];

    for (size_t i = 0; i < Size; i++) {
        int c = getchar();

        if (c >= '0' && c < '0' + Flash) {
            octopuses[i] = c - '0';
        }
        else {
            fputs("Invalid input: expected digit\n", stderr);
            return 1;
        }

        if (i % Width + 1 == Width && getchar() != '\n') {
            fputs("Invalid input: expected newline\n", stderr);
            return 1;
        }
    }

    int step = 0;

    int total_flashes = 0;

    for (; step < 100; step++) {
        total_flashes += step_octopuses(octopuses);
    }

    printf("%d\n", total_flashes);

    for (int flashes = 0; flashes != Size; step++) {
        flashes = step_octopuses(octopuses);
    }

    printf("%d\n", step);
}

int
step_octopuses(char *octopuses)
{
    int flashes = 0;

    for (size_t i = 0; i < Size; i++) {
        step_octopus(i, octopuses);
    }

    for (size_t i = 0; i < Size; i++) {
        octopuses[i] %= Flash;
        flashes += !octopuses[i];
    }

    return flashes;
}

void
step_octopus(size_t i, char *octopuses)
{
    if (octopuses[i] < Flash && ++octopuses[i] == Flash) {
        if (i >= Width) {
            if (i % Width >= 1) {
                step_octopus(i - Width - 1, octopuses);
            }
            step_octopus(i - Width, octopuses);
            if (i % Width + 1 < Width) {
                step_octopus(i - Width + 1, octopuses);
            }
        }
        if (i % Width >= 1) {
            step_octopus(i - 1, octopuses);
        }
        if (i % Width + 1 < Width) {
            step_octopus(i + 1, octopuses);
        }
        if (i + Width < Size) {
            if (i % Width >= 1) {
                step_octopus(i + Width - 1, octopuses);
            }
            step_octopus(i + Width, octopuses);
            if (i % Width + 1 < Width) {
                step_octopus(i + Width + 1, octopuses);
            }
        }
    }
}
