#include "wtypes.h"

int GetScreenWidth(void) {
  return GetSystemMetrics(SM_CXSCREEN);
}

int GetScreenHeight(void) {
  return GetSystemMetrics(SM_CYSCREEN);
}
