/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "extensions.h"

typedef struct s_extension_function{
  VALUE sym;
  extension_fptr ext_fun;
  struct s_extension_function* next;
} extension_function_t;

extension_function_t* extensions = NULL;

extension_fptr extensions_lookup(UINT sym) {
  extension_function_t *t = extensions;
  while (t != NULL) {
    if (t->sym == sym) {
      return t->ext_fun;
    }
    t = t->next;
  }
  return NULL;
}

bool extensions_add(char *sym_str, extension_fptr ext) {
  VALUE symbol;
  int res = symrepr_addsym(sym_str, &symbol);

  if (!res) return false;

  extension_function_t *extension = malloc(sizeof(extension_function_t));

  if (!extension) return false;

  extension->sym = symbol;
  extension->ext_fun = ext;
  extension->next = extensions;
  extensions = extension;
  return true;
}

void extensions_del(void) {
  extension_function_t *curr = extensions;
  extension_function_t *t;
  while (curr) {
    t = curr;
    curr = curr->next;
    free(t);
  }
  extensions = NULL;
}
