{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module CompositionalPattern where

handleRequest
  context
  Request
    { requestIdentifier = requestIdentifierWithLongName
    , requestPayload    =
      ( payloadAlias@(extractPayload -> extractedPayloadWithLongName),
        [firstPayloadPart, secondPayloadPart] )
    , requestMetadata
    , ..
    } =
    combine context
            requestIdentifierWithLongName
            extractedPayloadWithLongName
            requestMetadata

lambdaHandler = \ context
  Request
    { requestIdentifier = requestIdentifierWithLongName
    , requestMetadata   =
      Metadata
        { metadataOwner = metadataOwnerWithLongName
        , metadataTags  = [firstMetadataTag, secondMetadataTag]
        }
    , ..
    }
  -> combine context requestIdentifierWithLongName metadataOwnerWithLongName

caseHandler request = case request of
  Request
    { requestIdentifier = requestIdentifierWithLongName
    , requestPayload    = !strictPayloadWithLongName
    , requestMetadata   =
      ~(Metadata { metadataOwner = metadataOwnerWithLongName, ..})
    } ->
      combine requestIdentifierWithLongName
              strictPayloadWithLongName
              metadataOwnerWithLongName

guardHandler request
  | Request
    { requestIdentifier = guardedIdentifierWithLongName
    , requestMetadata = Metadata { metadataOwner = guardedOwnerWithLongName, ..}
    , ..
    } <- request
  = combine guardedIdentifierWithLongName guardedOwnerWithLongName

doHandler action = do
  Request
    { requestIdentifier = requestIdentifierWithLongName
    , requestPayload    = (payloadWithLongName :: Payload)
    , ..
    }     <- action
  pure requestIdentifierWithLongName

Request
  { requestIdentifier = boundRequestIdentifierWithLongName
  , requestMetadata   = boundRequestMetadataWithLongName
  } =
    defaultRequest

pattern LongRequest identifier metadata payload owner <-
  Request
    { requestIdentifier = identifier
    , requestMetadata   = metadata
    , requestPayload    = payload
    , requestOwner      = owner
    }

commented
  Request
    -- Keep this comment with the first field.
    { requestIdentifier = requestIdentifierWithLongName
    , requestMetadata   =
      Metadata { metadataOwner = metadataOwnerWithLongName, ..}
    } =
    requestIdentifierWithLongName

eitherRequest
  (Request
     { requestIdentifier = firstIdentifierWithLongName
     , requestMetadata   = firstMetadataWithLongName
     }
   ; Request
     { requestIdentifier = secondIdentifierWithLongName
     , requestMetadata   = secondMetadataWithLongName
     }) =
    result

compact Tiny { tinyOne, tinyTwo } = tinyOne
