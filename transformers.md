# Monad Transformers

#### Definition
From wikipedia (with modifications)
> In functional programming, a monad transformer is a type constructor
  which takes a monad as an argument and returns a monad as a result.
> * A type constructor `t` of kind that works on a monad type parameter
> * Monad operation `return` and `bind` satisfying monad laws
> * MonadTrans operation `lift :: m a -> t m a` satisfying monad transformer laws

#### First glimpse - MaybeT
```haskell
-- Control.Monad.Trans.Except
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```
> notes:
> 0. checkpoint: what is type constructor and value constructor
> 1. kind of type constructor MaybeT (on the left)
  is `(* -> *) -> * -> *`, where
  m has kind `* -> *`,
  a has kind `*`
  and MaybeT is a type constructor that takes a monad type constructor `m`,
  and type `a` to construct the final concrete type.
  In other words, `((* -> *) -> *) -> *` where `MaybeT m` has kind of
  `((* -> *) -> *)`! (Does this remind you of anything? Yes, a Monad!)
> 2. the key word `newtype` introduces a *single-parameter* value constructor.
  `data` can do the same thing, but `newtype` is considered more efficient
  than data, as
    - it forces value constructor to be strict, while data is lazy. eg.,
      - `newtype A = A Int` then `A undefined` will be evaluated to `undefined`
      whereas `data D = D Int` and `D undefined` is not.

##### Now what exactly is `MaybeT`?
Let us re-examine the kind of `MaybeT :: ((* -> *) -> *) -> *`,
now forget `MaybeT`,
instead we can think of `MaybeT m` as a type constructor for a monad.

##### Why a new monad?
(My) short story: clean code with composition.

(My) long story:
we want to transform monad `m a` to have the merits of `Maybe` monad.
This could be confusing.. So let us start with an example:
  - think of functions deal with two monads, one wrapped in another one.
  eg., `IO (Maybe a)`.
  In the function, your main logic only need to processes data of type `a`,
  but in order to reach `a`:
      1. you have to use *unwrap* and *wrap* for `IO` actions
      2. you have to do pattern matching for `Maybe`

    There are 2 nested layers of code. There could be many boilerplate
    code written for unwrapping and wrapping monads.
    (Imagine a huge monad stack!)
    ```haskell
    getValidData :: IO (Maybe String)
    processData1 :: String -> IO (Maybe String)
    processData2 :: String -> IO (Maybe String)

    outputData :: String -> IO ()
    outputError :: IO ()
    outputError = putStrLn "Invalid data!"

    foo :: IO ()
    foo :: IO ()
    foo = do
      maybe_data <- getValidData
      case maybe_data of
        Just some -> do
          maybe_data1 <- processData1 some
          case maybe_data1 of
            Just some -> do
              maybe_final_data <- processData2 some
              case maybe_final_data of
                Just some -> outputData some
                Nothing -> outputError
            Nothing -> outputError
        Nothing -> outputError

    -- what is going on here?
    -- so many nested pattern matchings on Maybe types,
    -- because IO monad does no know how to deal with Maybe!
    ```
    While looking at the types of process data functions,
    wouldn't it be nice to have a `>>=`? (Kleisli cat)
    Since `IO (Maybe String)` is not a monad, we need to
    define a monad that abstract `IO (Maybe String)`
    and provide us the `>>=`!

    So, what if there is a monad that has a function `>>=`
    which does `Maybe` pattern matching for you (short circuit),
    without having to *unwrap* and *wrap* `IO` using `do` notations?
    AKA, peek into `IO (Maybe a)`, and validates `Maybe a`,
    short circuit on Nothing.
    Here comes the `MaybeT IO` monad.
    ```haskell
    instance Monad m => Monad (MaybeT m) where
      return = MaybeT . return . Just

      -- The signature of (>>=), specialized to MaybeT m:
      -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
      x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> return Nothing
                               Just value -> runMaybeT $ f value
    ```
    > notes:
    > 1. in `return` function:
    `MaybeT` is the value constructor.
    `return` wraps things in monad `m`.
    `Just` wraps things in monad `Maybe`
    > 2. `>>=` bind, peeks into monad `IO`, applies `f` on value of type `a`
    and short circuit on `Nothing`. Now, it is obvious that this `>>=` *unwraps*
    2 layer of monads, and shovels the inner most value of type `a` and pass into
    `f`. But wait, you might start tp wonder now, okay, `>>=` *unwraps* 2 layers,
    but in order to use `>>=` for `Maybe T m` monad, I have to create `f` that
    *wraps* 2 layers of monads: `Maybe` and `IO`, is there an easy way to do
    that, so my efforts of using transformers are worthy?

  - Now, let us compose with the `>>=`
  ```haskell
  type NewMonad = MaybeT IO
  getValidData :: NewMonad String
  processData1 :: String -> NewMonad String
  processData2 :: String -> NewMonad String

  outputData :: String -> IO ()
  outputError :: IO ()
  outputError = putStrLn "Invalid data!"

  betterFoo :: IO ()
  betterFoo = do
      maybe_final_data <- runMaybeT $ getValidData >>= processData1 >>= processData2
      maybe outputError outputData maybe_final_data
  ```

  - Essentially, monad `MaybeT m a` generates a value of type `m (Maybe a)`,
  via `runMaybeT` function. It is used when you want to *unwrap* the outside
  monad.

  - `lift` allows us to reuse functions that work with `m` monad,
  and bring them into the `MaybeT m` monad in order to use them in `do` block
  ```haskell
  instance MonadTrans MaybeT where
      lift :: (Monad m, MonadTrans t) => m a -> t m a
      lift = MaybeT . (liftM Just)

  lift . return = return
  lift (m >>= f) = lift m >>= (lift . f)
  ```
  Let us use `lift` to make functions that produces `MaybeT IO` results
  from `IO` actions. It *wraps* the *content* of IO with `Just` and
  construct `MaybeT IO` monad.

  Imagine if we need to do `IO` actions in `getValidData`:
  ```haskell
  type NewMonad = MaybeT IO

  action1 :: IO String
  action2 :: IO String

  -- without using lift
  processData' :: String -> NewMonad String
  processData' str = MaybeT $ liftM Just result
      where
        result = do
          s1 <- action1
          s2 <- action2
          return $ str ++ s1 ++ s2

  -- using lift
  processData :: String -> NewMonad String
  processData str = do
      s1 <- lift action1
      s2 <- lift action2
      return $ str ++ s1 ++ s2
  ```

#### MonadTrans & MonadIO
The **transformers** package provides 2 useful classes:
- MonadTrans
- MonadIO

`MonadTrans` type class defines:
  - `lift :: Monad m, MonadTrans t => m a -> t m a`

`MonadIO` type class defines:
  - `liftIO :: Monad m => IO a -> m a`

> notes:
notice here, `liftIO` is a special case of `lift`,
ie, when `m` is `t m'`, which is the monad created by the transformer

#### Second Example - ExceptT

An `Either e a` wrapped in another monad `m`, ie,  `m (Either e a)`
```haskell
-- Control.Monad.Trans.Except
newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}
```
