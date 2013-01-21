#include "wtypes.h"

int screen_width(void) {
  return GetSystemMetrics(SM_CXSCREEN);
}

int screen_height(void) {
  return GetSystemMetrics(SM_CYSCREEN);
}
