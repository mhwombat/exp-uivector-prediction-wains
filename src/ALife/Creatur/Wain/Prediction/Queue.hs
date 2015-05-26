------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.Queue
-- Copyright   :  (c) Amy de Buitléir 2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A message queue for data mining.
--
------------------------------------------------------------------------
{-# OPTIONS -XOverloadedStrings #-}

import qualified Network.AMQP as Q
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (bracket)
import Data.Text (Text)

hostname :: String
hostname = "127.0.0.1"

virtualHost :: Text
virtualHost = "/"

loginName :: Text
loginName = "wain"

loginPassword :: Text
loginPassword = "wain"

queueName :: Text
queueName = "wain"

exchange :: Text
exchange = ""

openChannel :: IO (Q.Connection, Q.Channel)
openChannel = do
  conn <- Q.openConnection hostname virtualHost loginName loginPassword
  ch   <- Q.openChannel conn
  _ <- Q.declareQueue ch Q.newQueue { Q.queueName       = "hello",
                                      Q.queueAutoDelete = False,
                                      Q.queueDurable    = False }
  return (conn, ch)

closeChannel :: (Q.Connection, Q.Channel) -> IO ()
closeChannel (conn, _) = Q.closeConnection conn

sendMessage :: Q.Channel -> [Char] -> IO ()
sendMessage ch s = Q.publishMsg ch exchange queueName msg
  where msg = Q.newMsg { Q.msgBody = (BL.pack s), 
                       Q.msgDeliveryMode = Just Q.NonPersistent}

-- publishMsg chan exchange routingKey ms
-- getMsg chan ack queue
-- ackMsg chan deliveryTag

-- bracket
-- Source
-- :: IO a	

-- computation to run first ("acquire resource")
-- -> (a -> IO b)	

-- computation to run last ("release resource")
-- -> (a -> IO c)	

-- computation to run in-between
-- -> IO c	 

-- When you want to acquire a resource, do some work with it, and then release the resource, it is a good idea to use bracket, because bracket will install the necessary exception handler to release the resource in the event that an exception is raised during the computation. If an exception is raised, then bracket will re-raise the exception (after performing the release).

-- A common example is opening a file:

-- bracket
--   (openFile "filename" ReadMode)
--   (hClose)
--   (\fileHandle -> do { ... })


-- main :: IO ()
-- main = do

--      publishMsg ch "" "hello"
--                 (newMsg {msgBody         = (BL.pack "Hello World!"),
--                          msgDeliveryMode = Just NonPersistent})

--      putStrLn " [x] Sent 'Hello World!'"
--      closeConnection conn


-- {-# OPTIONS -XOverloadedStrings #-}

-- import Network.AMQP
-- import qualified Data.ByteString.Lazy.Char8 as BL

-- main :: IO ()
-- main = do

--      putStrLn " [*] Waiting for messages. to Exit press CTRL+C"
--      consumeMsgs ch "hello" NoAck deliveryHandler

--      -- waits for keypresses
--      getLine
--      closeConnection conn

-- deliveryHandler :: (Message, Envelope) -> IO ()
-- deliveryHandler (msg, metadata) = do
--   putStrLn $ " [x] Received " ++ (BL.unpack $ msgBody msg)
