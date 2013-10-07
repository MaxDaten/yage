# For GHCI Support
http://www.haskell.org/haskellwiki/WxHaskell/Mac

- Compile `EnableGUI.hs` with `ghc -XForeignFunctionInterface -c EnableGUI.hs`
- include EnableGUI in ex. `HelloWorld.hs` and use `enableGUI` (in your main)
- run with: `ghci -framework Carbon HelloWorld.hs`
- call `enableGUI >> main`



orig. text:
Due to complicated MacOS X restrictions, graphical wxHaskell applications do not work directly when used from GHCi. Fortunately, Wolfgang Thaller has kindly provided an ingenious Haskell module that solves this problem. Just import the (compiled) module EnableGUI in your program and issue the following command to run main from your GHCi prompt:

> enableGUI >> main
Compiling and using enableGUI needs some command line flags:

```
> ghc -XForeignFunctionInterface -c EnableGUI.hs
> ghci -framework Carbon HelloWorld.hs
GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help
Loading package base ... linking ... done.
Loading object (framework) Carbon ... done
final link ... done
[2 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main, EnableGUI.
*Main> enableGUI
*Main> main
```


ghc -main-is Yage.Core.GLFW --make GLFW.hs -o glfw