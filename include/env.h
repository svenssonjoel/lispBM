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

#ifndef ENV_H_
#define ENV_H_


extern int env_copy_shallow(uint32_t env, uint32_t *cpy);
extern int env_lookup(uint32_t sym, uint32_t env, uint32_t *res); 
extern int env_modify_binding(uint32_t env, uint32_t key, uint32_t val); 
extern int env_build_params_args(uint32_t params,
				 uint32_t args,
				 uint32_t env0,
				 uint32_t *res_env);
  

#endif 
