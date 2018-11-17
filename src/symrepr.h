/*
    Copyright 2018 Joel Svensson	svenssonjoel@yahoo.se

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

#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h> 

extern int symrepr_addsym(char *, uint32_t*); 
extern int symrepr_init(void);
extern void symrepr_print(void);
extern int symrepr_lookup(char *, uint32_t*);
extern char* symrepr_lookup_name(uint32_t); 
extern void symrepr_del(void);


extern uint32_t symrepr_nil(void);
extern uint32_t symrepr_quote(void);
extern uint32_t symrepr_car(void);
extern uint32_t symrepr_cdr(void);
extern uint32_t symrepr_cons(void);
extern uint32_t symrepr_lambda(void);
extern uint32_t symrepr_closure(void); 

extern uint32_t symrepr_rerror(void);
extern uint32_t symrepr_terror(void);
extern uint32_t symrepr_eerror(void);

#endif 
