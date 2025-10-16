#include <stdio.h>
#include <stdlib.h>

// Unix replacement for Windows _gcvt
char* _gcvt(double value, int digits, char* buffer) {
    if (!buffer) return NULL;
    snprintf(buffer, 64, "%.*g", digits, value); // 64-char buffer assumed
    return buffer;
}
