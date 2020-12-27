import Control.Exception (catch, throw)
import Data.Maybe (fromJust)
import Lib
import System.IO.Error (IOError, isDoesNotExistError)
import Test.Hspec (describe, it, parallel, pendingWith, shouldBe)
import Test.Hspec.Runner (hspec)

main :: IO ()
main =
    hspec $
    parallel $ do
        describe "part 1" $ do
            it "example 1" $ do
                let fileName = "./ex1_1.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> answer1 (parse1 contents) `shouldBe` ex1_1
            it "example 2" $ do
                let fileName = "./ex1_2.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> answer1 (parse1 contents) `shouldBe` ex1_2
            it "example 3" $ do
                let fileName = "./ex1_3.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> answer1 (parse1 contents) `shouldBe` ex1_3
            it "example 4" $ do
                let fileName = "./ex1_4.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> answer1 (parse1 contents) `shouldBe` ex1_4
            it "example 5" $ do
                let fileName = "./ex1_5.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> answer1 (parse1 contents) `shouldBe` ex1_5
        describe "part 2" $ do
            it "example 1" $ do
                let fileName = "./ex2_1.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> do
                        a <- answer2 (parse2 contents)
                        a `shouldBe` ex2_1
            it "example 2" $ do
                let fileName = "./ex2_2.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> do
                        a <- answer2 (parse2 contents)
                        a `shouldBe` ex2_2
            it "example 3" $ do
                let fileName = "./ex2_3.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> do
                        a <- answer2 (parse2 contents)
                        a `shouldBe` ex2_3
            it "example 4" $ do
                let fileName = "./ex2_4.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> do
                        a <- answer2 (parse2 contents)
                        a `shouldBe` ex2_4
            it "example 5" $ do
                let fileName = "./ex2_5.txt"
                mContents <-
                    (Just <$> readFile fileName) `catch`
                    (\e ->
                         if isDoesNotExistError e
                             then return Nothing
                             else throw (e :: IOError))
                case mContents of
                    Nothing ->
                        pendingWith ("File " <> fileName <> " does not exist")
                    Just contents -> do
                        a <- answer2 (parse2 contents)
                        a `shouldBe` ex2_5
