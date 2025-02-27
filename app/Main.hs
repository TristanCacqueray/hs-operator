{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVar)
import Control.Monad
import Kubernetes.Client (KubeConfigSource (..), mkKubeClientConfig)
import Kubernetes.OpenAPI (Accept (..), MimeJSON (..), dispatchMime)

import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core (configAuthMethods)

import Data.IORef qualified as IORef
import Data.Map qualified as Map
import Kubernetes.OpenAPI.API.CoreV1 qualified as CoreV1
import Kubernetes.OpenAPI.Core (KubernetesClientConfig)
import Kubernetes.OpenAPI.Model
import Network.HTTP.Client (Manager)

import Data.Text (Text)
import Data.Yaml
import Kubernetes.Client.KubeConfig qualified as KubeConfig

getDefaultNamespace :: IO Text
getDefaultNamespace = do
    config <- decodeFileThrow "/home/fedora/.kube/config"
    case KubeConfig.getContext config of
        Left err -> error $ "No context defined: " <> err
        Right ctx -> case ctx.namespace of
            Nothing -> error $ "No default namespace"
            Just ns -> pure ns

loadKubeConfig :: IO (Manager, KubernetesClientConfig)
loadKubeConfig = do
    oidcCache <- atomically $ newTVar $ Map.fromList []
    mkKubeClientConfig oidcCache $ KubeConfigFile "/home/fedora/.kube/config"

main :: IO ()
main = do
    let userProvidedNamespace = Nothing
    defaultNamespace <- case userProvidedNamespace of
        Nothing -> getDefaultNamespace
        Just n -> pure n
    (mgr, kcfg) <- loadKubeConfig
    dispatchMime
        mgr
        kcfg
        (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print . fmap (v1PodListMetadata) . mimeResult
