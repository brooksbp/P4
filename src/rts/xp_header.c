#include "xp_header.h"

void to_xp(uint8_t *bytes, uint8_t *xp, int offset, int nbits) {
  int bit;
  for (bit = 0; bit < nbits; bit++) {
    int header_bit = offset + nbits - 1 - bit;
    int header_bit_value = (bytes[header_bit / 8] >> (7 - (header_bit % 8))) & 0x01;
    xp[bit / 8] |= header_bit_value << (bit % 8);
  }
}

void from_xp(uint8_t *bytes, uint8_t *xp, int offset, int nbits) {
  int bit;
  for (bit = 0; bit < nbits; bit++) {
    int xp_bit_value = (xp[bit / 8] >> (bit % 8)) & 0x01;
    int header_bit = offset + nbits - 1 - bit;
    bytes[header_bit / 8] &= ~(1 << (7 - (header_bit % 8)));
    bytes[header_bit / 8] |= xp_bit_value << (7 - (header_bit % 8));
  }
}
