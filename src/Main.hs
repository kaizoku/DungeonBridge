{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Main where

import Network.IRC.Client

import Network

import Text.Printf
import System.IO as IO
import Data.Text as T
import Data.ByteString.Char8 as BS

import Control.Monad.State
import Control.Concurrent

import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)

irchost = "irc.paraphysics.net"
--irchost = "irc.shaw.ca"
ircport = 6667
botnick = "mudbot"
botchan = "#mud"

mudhost = "achaea.com"
mudport = 23


main :: IO ()
main = mdo
    mudcon <- connectMUD mudhost mudport
    (irccon, irccfg) <- connectIRC irchost ircport mudcon

    forkIO $ handleSocketMessages (recvMUD mudcon) (writeToQueue irccon)
    start irccon irccfg
    return ()


writeToQueue :: ConnectionConfig s -> Message ByteString -> IO ()
writeToQueue irccon msg = atomically (writeTBMChan (_sendqueue irccon) msg)


handleSocketMessages :: Monad m => m ByteString -> (Message ByteString -> m a) -> m b
handleSocketMessages input write = forever $ do
    incoming <- input
    write (Join (BS.pack botchan))
    let { msg = Privmsg (BS.pack botchan) (Right incoming) }
    write msg
    return ()



-----
-- IRC I/O
----------
connectIRC :: MonadIO m => ByteString -> Int -> Handle -> m (ConnectionConfig (), InstanceConfig ())
connectIRC host port mudcon = do
    conn <- connect host port 1
    let cfg = defaultIRCConf botnick
    let ehs = [ircInputHandler mudcon]
    let cfg' = cfg {
        _eventHandlers = ehs ++ _eventHandlers cfg,
        _channels = [T.pack botchan]
    }
    return (conn, cfg')

-- Handlers
printHandler :: EventHandler ()
printHandler = EventHandler "PRINT" EPrivmsg printStdout

printStdout :: UnicodeEvent -> IRC ()
printStdout ev =
    case _message ev of
        Privmsg chan (Right msg) -> liftIO $ IO.putStrLn (T.unpack msg)


ircInputHandler :: Handle -> EventHandler ()
ircInputHandler mudcon = EventHandler "Send MSGs to MUD" EPrivmsg (sendToMud mudcon)

sendToMud :: Handle -> UnicodeEvent -> IRC ()
sendToMud h ev =
    case _message ev of
        Privmsg chan (Right msg) -> liftIO $ sendMUD h (T.unpack msg)


----
-- MUD I/O
----------
connectMUD :: HostName -> Integer -> IO Handle
connectMUD host port = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h LineBuffering
    hPrintf h "Yarr\n"
    hPrintf h "password\n"
    hPrintf h "y\n"
    return h


sendMUD :: Handle -> String -> IO ()
sendMUD handle msg = do
    hPrintf handle "%s\n" msg
    hFlush handle

recvMUD :: Handle -> IO ByteString
recvMUD h = do
    input <- BS.hGetLine h
    BS.putStrLn input
    return input
