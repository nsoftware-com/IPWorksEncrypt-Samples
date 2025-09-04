<?php
/*
 * IPWorks Encrypt 2024 PHP Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks Encrypt in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksencrypt
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */
require_once('../include/ipworksencrypt_aes.php');
require_once('../include/ipworksencrypt_cast.php');
require_once('../include/ipworksencrypt_tripledes.php');
require_once('../include/ipworksencrypt_blowfish.php');
require_once('../include/ipworksencrypt_twofish.php');
require_once('../include/ipworksencrypt_ezcrypt.php');
require_once('../include/ipworksencrypt_const.php');
?>
<?php
if ($argc < 8) {
  echo "usage: php encrypt.php -a action -f inputfile -o outputfile [-w] -s inputstring -alg algorithm -p keypassword\n\n";
  echo "  -a action       chosen from {encrypt, decrypt}\n";
  echo "  -f inputfile    the path to the input file (specify this or inputstring, but not both)\n";
  echo "  -o outputfile   the path to the output file (specify if inputfile is specified)\n";
  echo "  -w              whether to overwrite the output file (optional)\n";
  echo "  -s inputstring  the message to encrypt or decrypt (if decrypt, must be in hex)\n";
  echo "  -alg algorithm  the symmetric encryption algorithm to use, chosen from\n";
  echo "                  {AES, Blowfish, CAST, ChaCha, DES, IDEA, RC2, RC4, Rijndael, TEA, TripleDES, Twofish, XSalsa20}\n";
  echo "  -p keypassword  the key password used to generate the Key and IV\n";
  echo "\nExample: php encrypt.php -a encrypt -f c:\\myfile.txt -o c:\\myencryptedfile.dat -w -alg aes -p password\n";
  return;
}
try{
  $ezcrypt = new IPWorksEncrypt_Ezcrypt();
  $action = "";
  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-alg") {
        switch (strtolower($argv[$i + 1])) {
          case "aes":
            $ezcrypt->setAlgorithm(0);
            break;
          case "blowfish":
            $ezcrypt->setAlgorithm(1);
            break;
          case "cast":
            $ezcrypt->setAlgorithm(2);
            break;
          case "chacha":
            $ezcrypt->setAlgorithm(11);
            break;
          case "des":
            $ezcrypt->setAlgorithm(3);
            break;
          case "idea":
            $ezcrypt->setAlgorithm(4);
            break;
          case "rc2":
            $ezcrypt->setAlgorithm(5);
            break;
          case "rc4":
            $ezcrypt->setAlgorithm(6);
            break;
          case "rijndael":
            $ezcrypt->setAlgorithm(10);
            break;
          case "tea":
            $ezcrypt->setAlgorithm(7);
            break;
          case "tripledes":
            $ezcrypt->setAlgorithm(8);
            break;
          case "twofish":
            $ezcrypt->setAlgorithm(9);
            break;
          case "xsalsa20":
            $ezcrypt->setAlgorithm(12);
            break;
          default:
            echo "Invalid algorithm selection.\n";
            return;
        }
      }
      if ($argv[$i] == "-a") {
        $action = strtolower($argv[$i + 1]);
      }
      if ($argv[$i] == "-f") {
        $ezcrypt->setInputFile($argv[$i + 1]);
      }
      if ($argv[$i] == "-s") {
        $ezcrypt->setUseHex(true);
        $ezcrypt->setInputMessage($argv[$i + 1]);
      }
      if ($argv[$i] == "-o") {
        $ezcrypt->setOutputFile($argv[$i + 1]);
        // if none specified, we'll output to the console (hex encoded)
      }
      if ($argv[$i] == "-w") {
        $ezcrypt->setOverwrite(true);
      }
      if ($argv[$i] == "-p") {
        $ezcrypt->setKeyPassword($argv[$i + 1]);
      }
    }
  }

  if ($action == "encrypt") {
    // encrypt
    if ($ezcrypt->getOutputFile() == "") { $ezcrypt->setUseHex(true); }
    $ezcrypt->doEncrypt();
  } elseif ($action == "decrypt") {
    // decrypt
    if ($ezcrypt->getInputFile() == "") { $ezcrypt->setUseHex(true); }
    $ezcrypt->doDecrypt();
  } else {
    echo "Invalid action.\n";
  }

  echo "Completed " . $action . "ion!\n";
  if ($ezcrypt->getOutputFile() == "") {
    echo "Output message: " . $ezcrypt->getOutputMessage() . "\n";
  } else {
    echo "Output file: " . $ezcrypt->getOutputFile() . "\n";
  }
} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>