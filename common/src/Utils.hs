{-# LANGUAGE CPP #-}

module Utils where

import Database.Groundhog.TH

#ifdef ghcjs_HOST_OS
ghCodeGen = defaultCodegenConfig
#else
ghCodeGen :: CodegenConfig
ghCodeGen = defaultCodegenConfig
    { namingStyle = lowerCaseSuffixNamingStyle }
#endif
