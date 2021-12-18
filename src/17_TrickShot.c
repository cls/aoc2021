#include <stdbool.h>
#include <stdio.h>

struct target {
    int left, right, bottom, top;
};

static bool launch(int x, int y, const struct target *target, int *max_peak);

int
main(int argc, char **argv)
{
    struct target target;

    if (scanf("target area: x=%d..%d, y=%d..%d\n", &target.left, &target.right, &target.bottom, &target.top) != 4) {
        fputs("Invalid input\n", stderr);
        return 1;
    }

    int max_max_y = 0;
    int successes = 0;

    for (int xd = 0; xd <= target.right; xd++) {
        for (int yd = target.bottom; yd <= -target.bottom; yd++) {
            successes += launch(xd, yd, &target, &max_max_y);
        }
    }

    printf("%d\n%d\n", max_max_y, successes);

    return 0;
}

bool
launch(int xd, int yd, const struct target *target, int *max_max_y)
{
    int x = 0;
    int y = 0;

    while (yd > 0) {
        x += xd;
        y += yd;
        xd -= (xd > 0) - (xd < 0);
        yd -= 1;
    }

    int max_y = y;

    while (x <= target->right && y >= target->bottom) {
        x += xd;
        y += yd;
        xd -= (xd > 0) - (xd < 0);
        yd -= 1;

        if (x >= target->left && x <= target->right && y >= target->bottom && y <= target->top) {
            if (*max_max_y < max_y) {
                *max_max_y = max_y;
            }
            return true;
        }
    }

    return false;
}
