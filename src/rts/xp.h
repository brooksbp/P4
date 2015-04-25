#ifndef XP_H_
#define XP_H_

#include <stdint.h>

/*
 * Arithmetic operations on extended integers of fixed precision.
 */

typedef uint8_t *xp_t;

/*
 * Compare - returns: <0 if x < y
 *                     0 if x == y
 *                    >0 if x > y
 */
int xp_cmp(int n, xp_t x, xp_t y);

#endif  /* XP_H_ */
