#include "xp.h"

int xp_cmp(int n, xp_t x, xp_t y) {
        int i = n - 1;

        while (i > 0 && x[i] == y[i])
                i--;
        return x[i] - y[i];
}
