module Chromatin.Test.Unit where

import Hedgehog (TestT)
import Ribosome.Test.Embed (TestConfig, Vars)
import Ribosome.Test.Unit (unitSpec)

import Chromatin.Data.Env (Env)
import Chromatin.Data.Error (Error)
import Chromatin.Test.Config (defaultTestConfig, defaultTestConfigWith)

type ChromatinTest a = TestT (Ribo Env Error) a

specConfig ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  TestConfig ->
  Env ->
  ChromatinTest () ->
  TestT m ()
specConfig =
  unitSpec

spec ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  Env ->
  ChromatinTest () ->
  TestT m ()
spec =
  specConfig defaultTestConfig

specWith ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  Env ->
  ChromatinTest () ->
  Vars ->
  TestT m ()
specWith e s vars =
  unitSpec (defaultTestConfigWith vars) e s

specWithDef ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  ChromatinTest () ->
  Vars ->
  TestT m ()
specWithDef = do
  specWith def
