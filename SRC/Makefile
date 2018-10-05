default: ../bin/syrup

../bin/syrup: BigArray.hs Bwd.hs Chk.hs Expt.hs HalfZip.hs Lex.hs Main.hs Par.hs Syn.hs Ty.hs Va.hs
	pushd ../.. ;  ghc --make -o Syrup/bin/syrup Syrup/SRC/Main.hs -main-is Syrup.SRC.Main.main ;  popd
