# Assignment 4

## Instructions
You should edit `src/Lib.hs`

## Testing
We've added a basic test suite so you can check the performance of your
parallelized mandelbrot set. This should make it easier to check your
implementation and make sure its run in parallel. I've also updated the
parallelism so that it defaults to running on as many cores as you have,
which might make the effects a bit more obvious. A couple notes:

You can run the default suite as follows
```bash
stack test
```

If you want to only test your parallel version, you can run
```bash
stack test --test-arguments "-m prefix runMandelPar"
```
which can be useful once you know how long `runMandelSeq` takes,
as the time won't change between runs.

Similarly, if wanted, you can do the same with `runMandelSeq` as the prefix
to only profile `runMandelSeq`.

### Testing Some of Part 2
As we've added `Handle` to the signatures, you can now run these for testing
output to the console by passing `stdout` (which is in `System.IO`) to these functions.

## Viewing your results
So, something that might not have been *entirely* clear is that a `.ppm` file
is a type of *image*. On macOS, there's default support for viewing these
with `Preview`. On Linux, I'd recommend `feh` on X11 or `imv` on Wayland,
though most things *should* support ppms. On Windows, however,
tragically the default image viewer doesn't support `ppm`s, to that 
end I'd recommend [this online viewer](https://www.cs.rhodes.edu/welshc/COMP141_F16/ppmReader.html),
which also has pretty nice errors for smaller mistakes. I've also heard Photoshop can open them,
though I haven't tested this myself.

## Migration
Most of the stuff in `Lib.hs` is the same, but there are a few changes I'll highlight. We've
changed `runMandelSeq` to take an `n` parameter and be pure instead of an `IO` action, closer
to how `runMandelPar` works. The `n` parameter allows us to pass it to `writeMandel`, which is
a slightly changed version of `writeMandelPar` which allows for running either the sequential
or the parallel version. These changes serve to make it easier to test. 

As far as methods you're implementing go, we've changed the type signature on a few of them.
```hs
runMandelPar :: Int -> [[Int]]
```
This is a pretty small change, and should allow you to avoid the use of
`fromIntegral` a bit more than previously.

We've added a parameter to `jose'sDVDEmporium` indicating the number of drivers,
as well as handles for in/out IO.
```hs
jose'sDVDEmporium :: Int -> Handle -> -> Handle -> IO ()
```

For a couple of the other part 2 functions, we've added `Handle` in (`deliver`, `takeOrders`). This is
so we can pass in either `stdout or `stdin`, or something else for the sake of testing.

We've also swapped `TVar Bool` to `TMVar ()` on `deliver`, as `TVar Bool`
was insufficient for synchronization and would thus result in jumbled output. `TMVar ()`
is pretty much like an `MVar ()` and you can treat it as such.

So to move your stuff, you just need to copy paste your methods for the most part.
The only differences being in your `runMandelPar` and some of the part 2 functions.
`runMandelPar`, you should have to change *very* little anyways.
# par hw Assignment 4 Haskell
# WeChat: cstutorcs

# QQ: 749389476

# Email: tutorcs@163.com

# Computer Science Tutor

# Programming Help

# Assignment Project Exam Help
