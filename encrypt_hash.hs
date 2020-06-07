-- Author: Yousif Dafalla
-- EECS-776 Final Project

--In order to be able to run this you need to install the following
--stack install cryptonite
--stack install ansi-terminal
--
--To run the program type the following command after installing cryptonite and ansi-terminal
--stack ghc encrypt_hash.hs

--
{-# LANGUAGE BlockArguments #-}

module Main where 
import System.Environment
import Crypto.Hash
import Data.ByteString (ByteString)
import System.Console.ANSI
import System.IO
import Crypto.Cipher.AES (AES256(..))
import Crypto.Cipher.Blowfish
import Crypto.Cipher.DES
import Crypto.Cipher.CAST5
import Crypto.Cipher.Types (ecbEncrypt, ecbDecrypt, cipherInit)
import Crypto.Error (CryptoError(..), eitherCryptoError)
import Data.ByteString.Char8 as B
import Crypto.Data.Padding

main = do	
	Prelude.putStrLn . id $ " "
	Prelude.putStrLn . id $ " "
	Prelude.putStrLn . id $ "What do you want to do:  "
 	Prelude.putStrLn . id $ "1. Hashing"
	Prelude.putStrLn . id $ "2. Symmetric Encryption"
	Prelude.putStrLn . id $ "3. Exit"	
	Prelude.putStr "Enter selection here: "
	hFlush stdout
	c <- Prelude.getLine
	if c == "3" then Prelude.putStrLn "Exiting" 
		    else if c == "1" || c == "2" then do 
						 {
						 Prelude.putStrLn " ";
						 Prelude.putStrLn . id $ "Pick either file or user input:  ";
					 	 Prelude.putStrLn . id $ "1. File";
						 Prelude.putStrLn . id $ "2. User Input from keyboard";
						 Prelude.putStr "Enter selection here: ";
						 hFlush stdout;
						 x <- Prelude.getLine;
						 Prelude.putStrLn " ";
						 contents <- if x == "1" then do {Prelude.putStr . id $"Enter file name: ";hFlush stdout;filename <- Prelude.getLine; (readAFile filename)}
							    		else if x == "2" then do {Prelude.putStr . id $ "Enter Input: ";hFlush stdout;B.getLine}
									     	else return (B.pack "null");
					
						if contents /= (B.pack "null") then
						 				if c == "1" then do{hashfunc contents;main} else do {crypto contents; main}
									       else do
										    {Prelude.putStrLn "Wrong selection, try again, ";main}
	 				         } 		
					       else do 
						     {Prelude.putStrLn "Wrong Selection, try again, "; main}
	
					      				


--Function for the user to pick Symmetic Encrypt Algorithm
crypto :: ByteString -> IO ()
crypto file = do
	Prelude.putStrLn . id $ " "
	Prelude.putStrLn . id $ "Which symmetric algorithm do you pick:  "
 	Prelude.putStrLn . id $ "1. AES256"
	Prelude.putStrLn . id $ "2. Blowfish256"
	Prelude.putStrLn . id $ "3. DES"
	Prelude.putStrLn . id $ "4. CAST5"		
	Prelude.putStr . id $ "Enter selection here: "
	hFlush stdout
	c <- Prelude.getLine
	Prelude.putStrLn . id $ " "
	Prelude.putStrLn . id $ "1. Encrypt"
	Prelude.putStrLn . id $ "2. Decrypt"
	Prelude.putStr . id $ "Enter selection here: "
	hFlush stdout
	x <- Prelude.getLine
	Prelude.putStrLn " "
	if c == "1" then 
			if x == "1" then 
					do{
						Prelude.putStr "Enter the encryption key (key must be 32 characters long = 256 bits): ";
						hFlush stdout;
						key <- B.getLine;
						Prelude.putStrLn " ";
						if ((B.length key) == 32) then 
										do {
										Prelude.putStr "Encrypted content: ";
										B.putStrLn (my_aesencrypt key file)
										   }
									else 
										do {
										Prelude.putStrLn "Key length not right, Please try again ";
										crypto file
										}
										
					  } 
				    else 
					do{
						Prelude.putStr "Enter the decryption key (key must be 32 characters long = 256 bits): ";
						hFlush stdout;
						key <- B.getLine;
						Prelude.putStrLn " ";
						if ((B.length key) == 32) then 
										do {
										Prelude.putStr "Decrypted content: ";
										B.putStrLn (my_aesdecrypt key file)
										   }
									else 
										do {
										Prelude.putStrLn "Key length not right, Please try again ";
										crypto file
										}
										
					  }
	    	    else 
			if c == "2" then 
					if x == "1" then 
						        do{
								Prelude.putStr "Enter the encryption key (key must be 32 characters long = 256 bits): ";
								hFlush stdout;
								key <- B.getLine;
								Prelude.putStrLn " ";
								if ((B.length key) == 32) then 
												do {
												Prelude.putStr "Encrypted content: ";
												B.putStrLn (my_Blowfishencrypt key file)
												   }
											  else 
												do {
												Prelude.putStrLn "Key length not right, Please try again ";
												crypto file
											           }
											
						          } 
						    else 
							do{
								Prelude.putStr "Enter the decryption key (key must be 32 characters long = 256 bits): ";
								hFlush stdout;
								key <- B.getLine;
								Prelude.putStrLn " ";
								if ((B.length key) == 32) then 
												do {
												Prelude.putStr "Decrypted content: ";
												B.putStrLn (my_Blowfishdecrypt key file)
												   }
											else 
												do {
												Prelude.putStrLn "Key length not right, Please try again ";
												crypto file
												   }
					                  }


		             	    else if c == "3" then 
							if x == "1" then 
								        do{
										Prelude.putStr "Enter the encryption key (key must 8 characters long): ";
										hFlush stdout;
										key <- B.getLine;
										Prelude.putStrLn " ";
										if ((B.length key)==8) then 
														do {
														Prelude.putStr "Encrypted content: ";
														B.putStrLn (my_desencrypt key file)
														   }
													  else 
														do {
														Prelude.putStrLn "Key length not right, Please try again ";
														crypto file
													           }
													
								          } 
								    else 
									do{
										Prelude.putStr "Enter the decryption key (key must be 8 characters long): ";
										hFlush stdout;
										key <- B.getLine;
										Prelude.putStrLn " ";
										if ((B.length key) == 8) then 
														do {
														Prelude.putStr "Decrypted content: ";
														B.putStrLn (my_desdecrypt key file)
														   }
													else 
														do {
														Prelude.putStrLn "Key length not right, Please try again ";
														crypto file
														   }
					                                  }

						      else 
							if c == "4" then 
									if x == "1" then 
	
										        do{
												Prelude.putStr "Enter the encryption key (key must between 8 and 16 characters long): ";
												hFlush stdout;
												key <- B.getLine;
												Prelude.putStrLn " ";
												if (((B.length key)>=8) && ((B.length key)<=16)) then 
																do {
																Prelude.putStr "Encrypted content: ";
																B.putStrLn (my_cat5encrypt key file)
																   }
															  else 
																do {
																Prelude.putStrLn "Key length not right, Please try again ";
																crypto file
															           }
															
										          } 
										    else 
											do{
												Prelude.putStr "Enter the decryption key (key must be 8 and 16 characters long): ";
												hFlush stdout;
												key <- B.getLine;
												Prelude.putStrLn " ";
												if (((B.length key)>=8) && ((B.length key)<=16)) then 
																do {
																Prelude.putStr "Decrypted content: ";
																B.putStrLn (my_cat5decrypt key file)
																   }
															else 
																do {
																Prelude.putStrLn "Key length not right, Please try again ";
																crypto file
																   }
					                                  	  	  }

								    else 
									do{Prelude.putStrLn "Wrong, selection try again";crypto file}

--Read a file
readAFile :: String -> IO ByteString
readAFile file = do
	dump <- (B.readFile file)
	return dump


--Read user input
readUserInput :: IO ByteString
readUserInput = do	
	Prelude.putStrLn . id $ ""
	Prelude.putStr . id $ "Enter input: " 	
	input <- B.getLine
	return input	

--functional layer
--Function for the user to pick hashing algorithm
hashfunc :: ByteString -> IO ()
hashfunc file = do
	Prelude.putStrLn . id $ " "
	Prelude.putStrLn . id $ "Which hashing algorithm do you pick:  "
 	Prelude.putStrLn . id $ "1. Sha1"
	Prelude.putStrLn . id $ "2. Sha256"
	Prelude.putStrLn . id $ "3. MD5"
	Prelude.putStrLn . id $ "4. MD4"
	Prelude.putStr . id $ "Enter selection here: "
	hFlush stdout	
	c <- Prelude.getLine
	Prelude.putStrLn " "
	if c == "1" then sha1 file 
	    	    else if c == "2" then sha256 file
		             	     else if c == "3" then md5 file
	                              			else if c == "4" then md4 file	
									 else	
										do
										clearScreen
										(Prelude.putStrLn . id $ "Wrong selection, try again")
										hashfunc file	

-- SHA1, SHA256, MD5 and MD4 Hashing Algorithms
sha1 :: ByteString -> IO ()
sha1 msg = do
   Prelude.putStrLn $ "SHA1 hash is: " ++ show (hashWith SHA1 msg)


sha256 :: ByteString -> IO ()
sha256 msg = do
   Prelude.putStrLn $ "SHA256 hash is: " ++ show (hashWith SHA256 msg)


md5 :: ByteString -> IO ()
md5 msg = do
   Prelude.putStrLn $ "MD5 hash is:  " ++ show (hashWith MD5 msg)


md4 :: ByteString -> IO ()
md4 msg = do
   Prelude.putStrLn $ "MD4 hash is: " ++ show (hashWith MD4 msg)

--Symmetric Encryption
makeSecretKey :: ByteString -> Either CryptoError Blowfish256
makeSecretKey secret = eitherCryptoError (cipherInit secret)


makeSecretAES :: ByteString -> Either CryptoError AES256
makeSecretAES secret = eitherCryptoError (cipherInit secret)


makeSecretDES :: ByteString -> Either CryptoError DES
makeSecretDES secret = eitherCryptoError (cipherInit secret)

makeSecretCat5 :: ByteString -> Either CryptoError CAST5
makeSecretCat5 secret = eitherCryptoError (cipherInit secret)


cat5_key :: ByteString
cat5_key = B.pack "987-654-"

encryptBlowFishECB :: Blowfish256 -> ByteString -> ByteString
encryptBlowFishECB = ecbEncrypt

decryptBlowFishECB :: Blowfish256 -> ByteString -> ByteString
decryptBlowFishECB secKey msg = ecbDecrypt secKey msg

encryptAESECB :: AES256 -> ByteString -> ByteString
encryptAESECB = ecbEncrypt

decryptAESECB :: AES256 -> ByteString -> ByteString
decryptAESECB = ecbDecrypt


encryptDESECB :: DES -> ByteString -> ByteString
encryptDESECB = ecbEncrypt

decryptDESECB :: DES -> ByteString -> ByteString
decryptDESECB = ecbDecrypt

encryptCAT5ECB :: CAST5 -> ByteString -> ByteString
encryptCAT5ECB = ecbEncrypt

decryptCAT5ECB :: CAST5 -> ByteString -> ByteString
decryptCAT5ECB = ecbDecrypt

my_Blowfishencrypt :: ByteString -> ByteString -> ByteString
my_Blowfishencrypt secret str = 
	let Right key = makeSecretKey secret in (encryptBlowFishECB (key) (pad (PKCS7 16) (str)))

my_Blowfishdecrypt :: ByteString -> ByteString -> ByteString
my_Blowfishdecrypt secret str = 
	let Right key = makeSecretKey secret in (decryptBlowFishECB (key) (pad (PKCS7 16) (str)))	




my_aesencrypt :: ByteString -> ByteString -> ByteString
my_aesencrypt secret str = 
	let Right key = makeSecretAES secret in (encryptAESECB (key) (pad (PKCS7 16) (str)))

my_aesdecrypt :: ByteString -> ByteString -> ByteString
my_aesdecrypt secret str = 
	let Right key = makeSecretAES secret in (decryptAESECB (key) (pad (PKCS7 16) (str)))	



my_desencrypt :: ByteString -> ByteString -> ByteString
my_desencrypt des_key str = 
	let Right key = makeSecretDES des_key in (encryptDESECB (key) (pad (PKCS7 16) (str)))

my_desdecrypt :: ByteString -> ByteString -> ByteString
my_desdecrypt des_key str = 
	let Right key = makeSecretDES des_key in (decryptDESECB (key) (pad (PKCS7 16) (str)))	


my_cat5encrypt :: ByteString -> ByteString -> ByteString
my_cat5encrypt cat5_key str = 
	let Right key = makeSecretCat5 cat5_key in (encryptCAT5ECB key (pad (PKCS7 16) (str)))


my_cat5decrypt :: ByteString -> ByteString -> ByteString
my_cat5decrypt cat5_key str = 
	let Right key = makeSecretCat5 cat5_key in (decryptCAT5ECB key str)
