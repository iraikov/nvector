
fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

fun realRandomArray (xseed,yseed) size =
    let 
        val seed   = Random.rand (xseed,yseed)
        val a      = Real64Array.array(size, Random.randReal seed)
        fun loop 0 = a
          | loop j = (Real64Array.update(a, size-j, Random.randReal seed);
                      loop (j-1))
    in loop (size - 1)
    end

val _ = (let open NVector
             val n = 4
	     val v = VMake (n, Real64Array.fromList [1.0,2.0,3.0,4.0])
	     val v1 = VAbs v
	     val v3 = VLinearSum (1.0,v,2.0,v1)
	     val v4 = VLinearSum (3.0,v1,4.0,v3)
	 in
             putStrLn "v = ";
	     VPrint v;
             putStrLn "v1 = ";
	     VPrint v1;
             putStrLn "v3 = ";
	     VPrint v3;
             putStrLn "v4 = ";
	     VPrint v4
	 end)
