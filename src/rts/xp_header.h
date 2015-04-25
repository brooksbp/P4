#ifndef XP_HEADER_H_
#define XP_HEADER_H_

#include <stdint.h>

void to_xp(uint8_t *bytes, uint8_t *xp, int offset, int nbits);
void from_xp(uint8_t *bytes, uint8_t *xp, int offset, int nbits);

#endif  /* XP_HEADER_H_ */
