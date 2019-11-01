{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Example.UtilisationMTrans
-- Copyright   :  Benoît Fraikin 2019
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- Exemple de l'impact et de l'importance des transformateur de monades
--
-----------------------------------------------------------------------------

module Example.UtilisationMTrans (
    -- * Utilisation
      run
    -- * Système sans transformateur de monade
    , EtatControlé
    , eval
    , incrémente
    , nFoisErroné
    , nFois
    -- * Système avec transformateur de monade
    , EtatControléT
    , evalT
    , (***)
    -- ** Avec l'identité
    , incrémenteI
    , nFoisI
    -- ** avec IO
    , EtatControléIO
    , incrémenteIO
    , nFoisIO
    ) where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text
import           System.Environment
import           System.IO              (hPutStr)
import           Text.Read              (readMaybe)

run = do
    args <- getArgs
    (constante, nombre) <- analyse args

    putStrLn ""
    putStrLn "======> Etat Controlé Erroné"
    putStrLn $ "« incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn $ unpack $ eval incrémente constante
    putStrLn $ "2 « incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn $ unpack $ eval (incrémente >> incrémente) constante
    putStrLn $ "nFois (0)" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn $ unpack $ eval (nFoisErroné 0) constante
    putStrLn $ "nFois (" ++ show nombre ++ ")" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn $ unpack $ eval (nFoisErroné nombre) constante

    putStrLn ""
    putStrLn "======> Etat Controlé"
    putStrLn $ "« incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn $ unpack $ eval incrémente constante
    putStrLn $ "2 « incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn $ unpack $ eval (incrémente >>= (\s0 -> (incrémente >>= \s1 -> return (s0 >> s1)))) constante
    putStrLn $ "nFois (0)" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn $ unpack $ eval (nFois 0) constante
    putStrLn $ "nFois (" ++ show nombre ++ ")" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn $ unpack $ eval (nFois nombre) constante

    putStrLn ""
    putStrLn "======> Etat Controlé (avec transformateur) dans Identity"
    putStrLn $ "« incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn $ unpack $ runIdentity $ evalT incrémenteI constante
    putStrLn $ "2 « incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn $ unpack $ runIdentity $ evalT (incrémenteI >> incrémenteI) constante
    putStrLn $ "nFois (0)" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn $ unpack $ runIdentity $ evalT (nFoisI 0) constante
    putStrLn $ "nFois (" ++ show nombre ++ ")" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn $ unpack $ runIdentity $ evalT (nFoisI nombre) constante

    putStrLn ""
    putStrLn "======> Etat Controlé (avec transformateur) dans IO"
    putStrLn $ "« incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn . unpack =<< evalT incrémenteIO constante
    putStrLn $ "2 « incrémente » de " ++ show constante ++ " à partir de 0"
    putStrLn . unpack =<< evalT ( (liftIO $ putStrLn "Premier appel")
                                >> incrémenteIO
                                >> (liftIO $ putStrLn "deuxième appel")
                                >> incrémenteIO
                                ) constante
    putStrLn $ "nFois (0)" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn . unpack =<< evalT (nFoisIO 0) constante
    putStrLn $ "nFois (" ++ show nombre ++ ")" ++ " « incrémente » de " ++ show constante ++ " à partir de 0 :"
    putStrLn . unpack =<< evalT (nFoisIO nombre) constante

  where analyse args = case (treat args) of
                           (Nothing, Nothing) -> do c <- askForConstante
                                                    n <- askForNumber
                                                    return (c,n)
                           (Just c, Nothing) -> do n <- askForNumber
                                                   return (c,n)
                           (Nothing, Just n) -> do c <- askForConstante
                                                   return (c,n)
                           (Just c, Just n) -> return (c,n)
        --
        base = (Nothing, Nothing)
        --
        treat (opt:args)
            | opt == "-n" || opt == "--nombre" = case args of
                                                     [] -> base
                                                     _  -> let (c, _) = treat $ Prelude.tail args
                                                               n = readMaybe $ Prelude.head args
                                                           in (c, n)
            | opt == "-c" || opt == "--constante" = case args of
                                                        [] -> base
                                                        _  -> let (_, n) = treat $ Prelude.tail args
                                                                  c = readMaybe $ Prelude.head args
                                                              in (c, n)
            | otherwise = error "unknown args"
        treat [] = base
        --
        askForConstante = do putStrLn "Entrer un entier positif pour la constante"
                             c <- getLine
                             let constante = read c
                             return constante
        askForNumber = do putStrLn "Entrer un entier positif pour le nombre de répetition"
                          n <- getLine
                          let nombre = read n
                          return nombre


-- | 'EtatControlé' est une monade construite à partir de 'Reader' et 'State'.
--
--   * La monade @'Reader' e a@ est isomorphique à @e -> a@
--   * La monade @'State' s a@ est isomorphique à @s -> (a, s)@
--
--   La monade 'EtatControlé' est donc isomorphique à @e -> s -> (a, s)@
--
type EtatControlé = Reader Int (State Int Text)


eval :: EtatControlé -> Int -> Text
eval r n = let s = runReader r n
           in evalState s 0


incrémente :: EtatControlé
incrémente = do constante <- ask -- l'environnement est récupéré de 'Reader'
                -- Il faut maintenant produire un résultat 'State'
                return $ do variable <- get -- la variable est extraite de 'State'
                            let nouvelleVariable = variable + constante
                            put nouvelleVariable
                            -- la valeur fournie par 'State' est produite ici et
                            -- sera fournir par la fonction 'return' pour la
                            -- monade 'Reader'
                            return $ pack $ show nouvelleVariable


-- | La fonction 'nFoisErroné' veut composer @n@ itérations de 'incrémente'.
-- C'est une version naïve de 'nFois' qui ne fonctionne pas (cf. analyse).
-- On peut la comparer à 'nFoisT'
--
nFoisErroné :: Int -> EtatControlé
nFoisErroné n
    | n > 1 = incrémente >> nFoisErroné (n - 1)
    | n == 1 = incrémente
    | otherwise = return $ put 0 >> return "0"


-- | La fonction 'nFois' compose @n@ itérations de 'incrémente'.
-- La monade est bien construite. Mais on voit l'obligation la façon compliqué d'extraire les monades @'State' 'Int'@
-- résultant de la monade 'EtatControlé'
--
nFois :: Int -> EtatControlé
nFois n
    | n > 1 = incrémente >>= \ etat_inc ->
                do etat_nMoins1 <- nFois (n - 1)
                   return $ etat_inc >> etat_nMoins1
    | n == 1 = incrémente
    | otherwise = return $ put 0 >> return "0"


-- | 'EtatControléT' utilise un transformateur de monade pour 'Reader'.
--
--   * La monade @'ReaderT' e m a@ est isomorphique à @e -> m a@
--   * La monade @'State' s a@ est isomorphique à @s -> (a, s)@
--
--   La monade 'EtatControléT' est donc isomorphique à @e -> s -> (a, s)@.
--
--   En cela, il n'y a pas de différence de structure et de propriété entre
--   'EtatControlé' et 'EtatControléT'.
--
type EtatControléT m = ReaderT Int (StateT Int m) Text


evalT :: Monad m => EtatControléT m -> Int -> m Text
evalT r n = evalStateT (runReaderT r n) 0

(***) :: Monad m => Int -> EtatControléT m -> EtatControléT m
(***) 0 _ = put 0 >> return "0"
(***) 1 e = e
(***) n e = e >> (***) (n-1) e

-- | La monade 'incrémenteI' correspond à 'incrémente' dont le résultat
--   est encapsulée dans la monade 'Identity'.
--   La monade 'incrémenteI' est donc isomorphe à 'incrémente'.
incrémenteI :: EtatControléT Identity
incrémenteI = do constante <- ask -- l'environnement est récupéré de 'Reader'
                 -- Etant dans une monade transformée, la monade 'State' est directement accessible
                 variable <- get -- la variable est extraite de 'State'
                 let nouvelleVariable = variable + constante
                 put nouvelleVariable
                 -- la fonction 'return' produit la valeur fournie par la monade transformée
                 -- @'Reader' 'Int' ('State' 'Int')@ dans laquelle évolue le calcul
                 return $ pack $ show nouvelleVariable


-- | La fonction 'nFoisI' compose @n@ itérations de 'incrémenteI'.
--
nFoisI :: Int -> EtatControléT Identity
nFoisI n = n *** incrémenteI
{-
nFoisI n
    | n > 1 = incrémenteI >> nFoisI (n - 1)
    | n == 1 = incrémenteI
    | otherwise = put 0 >> return "0"
-}

-- | 'EtatControléIO' est l'encapsulation d'un état controlé dans la monade IO
type EtatControléIO = EtatControléT IO


-- | La monade 'incrémenteIO' ...
incrémenteIO :: EtatControléIO
incrémenteIO = do constante <- ask -- l'environnement est récupéré de 'Reader'
                  -- Etant dans une monade transformée, la monade 'State' est directement accessible
                  variable <- get -- la variable est extraite de 'State'
                  let nouvelleVariable = variable + constante
                  put nouvelleVariable
                  -- la fonction 'return' produit la valeur fournie par la monade transformée
                  -- @'Reader' 'Int' ('State' 'Int')@ dans laquelle évolue le calcul
                  return $ pack $ show nouvelleVariable


-- | La fonction 'nFoisIO' compose @n@ itérations de 'incrémenteIO'.
nFoisIO :: Int -> EtatControléIO
nFoisIO n = n *** (get >>= affiche >> incrémenteIO)
    where affiche i = liftIO . putStrLn $ "valeur temporaire : " ++ show i

-- Diagnostic
-- Le problème avec EtatControlé est que la monade est (Reader Int). Le lien crée par >>
-- transmet l'environnement mais seulement l'environnement.  C'est la raison pour laquelle
-- il faut récupérer l'état interne (sous la forme d'une monade State Int) et utiliser >>=
-- pour le transmettre à l'autre monade (Reader Int) de laquelle il faudra aussi extraire
-- l'état résultant (et le réencapsuler en monade (Reader Int)).
-- L'utilisation des transformateurs de monade (et de EtatControléT) cache ce mécanisme.
-- En effet, la monade est maintenant (Reader Int (State Int))


