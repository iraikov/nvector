
(* 
 * nvector.sml
 *
 *
 * Copyright 2009-2016 Ivan Raikov
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * A full copy of the GPL license can be found at
 * <http://www.gnu.org/licenses/>.
 *
 *)


signature NVECTOR  =
sig
    include MLTON_POINTER

    type nvector = t

    val VNew: int -> nvector

    val VMake: Real64Array.array -> nvector

    val VClone: nvector -> nvector

    val VPrint: nvector -> unit

    val VSub: int * nvector -> real
                                   
    val VSet: int * real * nvector -> unit

    (* z = a x + b y *)
    val VLinearSum: real * nvector * real * nvector -> nvector

    (* z[i] = c for i=0, 1, ..., N-1 *)
    val VConst: real * nvector -> nvector

    (* z[i] = x[i] * y[i] for i=0, 1, ..., N-1 *)
    val VProd: nvector * nvector -> nvector 

    (* z[i] = x[i] / y[i] for i=0, 1, ..., N-1 *)
    val VDiv: nvector * nvector -> nvector

    (* z = c x *)
    val VScale: real * nvector -> nvector

    (* z[i] = |x[i]| for i=0, 1, ..., N-1 *)
    val VAbs: nvector -> nvector

(* z[i] = 1.0 / x[i] for i = 0, 1, ..., N-1

 NOTE: This routine does not check for division by 0. It should be
       called only with an N_Vector x which is guaranteed to have all
       non-zero components.
 *)
    val VInv: nvector -> nvector

    (* z[i] = x[i] + b   for i = 0, 1, ..., N-1 *)

    val VAddConst: nvector * real -> nvector

    (* sum (i=0 to N-1) {x[i] * y[i]} *)
    val VDotProd: nvector -> real

    (* max (i=0 to N-1) |x[i]| *)
    val VMaxNorm: nvector -> real

    (* sqrt [(sum (i=0 to N-1) {(x[i] * w[i])^2}) / N] *)
    val VWrmsNorm: nvector * nvector -> real

(* sqrt [(sum (i=0 to N-1) {(x[i] * w[i] * msk[i])^2}) / N]
   where msk[i] = 1.0 if id[i] > 0 and
         msk[i] = 0.0 if id[i] < 0
*)
    val VWrmsNormMask: nvector * nvector * nvector -> real

    (* min (i=0 to N-1) x[i] *)
    val VMin: nvector -> real

    (* sqrt [(sum (i=0 to N-1) {(x[i] * w[i])^2}) ] *)
    val VWL2Norm: nvector * nvector -> real

(* z[i] = 1.0 if |x[i]| >= c   i = 0, 1, ..., N-1
                  0.0 otherwise *)
    val VCompare: real * nvector -> nvector

end


structure NVector: NVECTOR =
struct
    open MLton.Pointer

    type nvector = t
                       
    fun app (a, b, f) =
        if a < b then
            (f a; app (a+1, b, f))
        else
            ()

    val VNew = _import "N_VNew_Serial" : int -> nvector;

    val VClone = _import "N_VClone" : nvector -> nvector;

    val VPrint = _import "N_VPrint_Serial": nvector -> unit;

    val VSub = _import "N_VSub" : int * nvector -> real;

    val VSet = _import "N_VSet" : int * real * nvector -> unit;

    fun VMake (v: Real64Array.array) = 
                                                          
        let val n = Real64Array.length v
            val res = VNew n
        in
            app (0, n, fn(i) => VSet (i, Real64Array.sub(v, i), res)); res
        end


    val CVLinearSum = _import "N_VLinearSum_Serial": real * nvector * real * nvector * nvector -> unit;
    fun VLinearSum (a: real, x: nvector, b: real, y: nvector) =
	let 
	    val z = VClone x
	in
	    CVLinearSum (a,x,b,y,z); z
	end
										       
    (* z[i] = c for i=0, 1, ..., N-1 *)
    val CVConst = _import "N_VConst_Serial" : real * nvector -> unit;
    fun VConst (c: real, v: nvector) =
	let 
	    val z = VClone v
	in
	    CVConst (c, z); z
	end

    (* z[i] = x[i] * y[i] for i=0, 1, ..., N-1 *)
    val CVProd = _import "N_VProd_Serial": nvector * nvector * nvector -> unit ;
    fun VProd (x: nvector, y: nvector) =
	let 
	    val z = VClone x
	in
	    CVProd (x, y, z); z
	end

    (* z[i] = x[i] / y[i] for i=0, 1, ..., N-1 *)
    val CVDiv = _import "N_VDiv_Serial":  nvector * nvector * nvector -> unit;
    fun VDiv (x: nvector, y: nvector) =
	let 
	    val z = VClone x
	in
	    CVDiv (x, y, z); z
	end

    (* z = c x *)
    val CVScale = _import "N_VScale_Serial":  real * nvector * nvector -> unit;
    fun VScale (c: real, v: nvector) =
	let 
	    val z = VClone v
	in
	    CVScale (c, v, z); z
	end

    (* z[i] = |x[i]| for i=0, 1, ..., N-1 *)
    val CVAbs = _import "N_VAbs_Serial":  nvector * nvector -> unit;
    fun VAbs (v: nvector) =
	let 
	    val z = VClone v
	in
	    CVAbs (v, z); z
	end


    (* z[i] = 1.0 / x[i] for i = 0, 1, ..., N-1 *)
    val CVInv = _import "N_VInv_Serial": nvector * nvector -> unit;
    fun VInv (v: nvector) =
	let 
	    val z = VClone v
	in
	    CVInv (v, z); z
	end

    (* z[i] = x[i] + b   for i = 0, 1, ..., N-1 *)
    val CVAddConst = _import "N_VAddConst_Serial": nvector * real * nvector -> unit;
    fun VAddConst (v: nvector, b: real) =
	let 
	    val z = VClone v
	in
	    CVAddConst (v, b, z); z
	end

    (* sum (i=0 to N-1) {x[i] * y[i]} *)
    val VDotProd = _import "N_VDotProd_Serial": nvector -> real;


    (* max (i=0 to N-1) |x[i]| *)
    val VMaxNorm = _import "N_VMaxNorm_Serial": nvector  -> real;

    (* sqrt [(sum (i=0 to N-1) {(x[i] * w[i])^2}) / N] *)
    val VWrmsNorm = _import "N_VWrmsNorm_Serial": nvector * nvector -> real;


(* sqrt [(sum (i=0 to N-1) {(x[i] * w[i] * msk[i])^2}) / N]
   where msk[i] = 1.0 if id[i] > 0 and
         msk[i] = 0.0 if id[i] < 0
*)
    val VWrmsNormMask = _import "N_VWrmsNormMask_Serial": nvector * nvector * nvector -> real;

    (* min (i=0 to N-1) x[i] *)
    val VMin = _import "N_VMin_Serial": nvector -> real;
			 

    (* sqrt [(sum (i=0 to N-1) {(x[i] * w[i])^2}) ] *)
    val VWL2Norm = _import "N_VL2Norm_Serial": nvector * nvector -> real;

(* z[i] = 1.0 if |x[i]| >= c   i = 0, 1, ..., N-1
                  0.0 otherwise *)
    val CVCompare = _import "N_VCompare_Serial": real * nvector * nvector -> unit;
    fun VCompare (c: real, v: nvector) =
	let 
	    val z = VClone v
	in
	    CVCompare (c, v, z); z
	end

end
