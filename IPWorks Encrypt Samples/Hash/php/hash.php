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
require_once('../include/ipworksencrypt_hash.php');
require_once('../include/ipworksencrypt_const.php');
?>
<?php
if ($argc < 3) {
  echo "usage: php hash.php -f inputFile -s inputString -hex -alg algorithm\n\n";
  echo "  -f           the path to the input file (specify this or input string, but not both)\n";
  echo "  -s           the message to hash\n";
  echo "  -hex         whether to hax encode the hash value (optional)\n";
  echo "  -alg         the hash algorithm to use, chosen from:\n";
  echo "               {SHA1, SHA224, SHA256, SHA384, SHA512, MD2, MD4, MD5, RIPEMD160, MD5SHA1, HMACMD5, HMACSHA1,\n";
  echo "                HMACSHA224, HMACSHA256, HMACSHA384, HMACSHA512, HMACRIPEMD160, SHA3-224, SHA3-256, SHA3-384,\n";
  echo "                SHA3-512, SHA512-224, SHA512-256}\n";
  echo "\nExample: php hash.php -f C:\\myfile.txt -hex -alg sha256\n";
  return;
}

try{
  $hash = new IPWorksEncrypt_Hash();
  $hash->setEncodeHash(false); // only encode if -hex is specified

  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-alg") {
        switch (strtoupper($argv[$i + 1])) {
          case "SHA1":
            $hash->setAlgorithm(0);
            break;
          case "SHA224":
            $hash->setAlgorithm(1);
            break;
          case "SHA256":
            $hash->setAlgorithm(2); // default
            break;
          case "SHA384":
            $hash->setAlgorithm(3);
            break;
          case "SHA512":
            $hash->setAlgorithm(4);
            break;
          case "MD2":
            $hash->setAlgorithm(5);
            break;
          case "MD4":
            $hash->setAlgorithm(6);
            break;
          case "MD5":
            $hash->setAlgorithm(7);
            break;
          case "RIPEMD160":
            $hash->setAlgorithm(8);
            break;
          case "MD5SHA1":
            $hash->setAlgorithm(9);
            break;
          case "HMACMD5":
            $hash->setAlgorithm(10);
            break;
          case "HMACSHA1":
            $hash->setAlgorithm(11);
            break;
          case "HMACSHA224":
            $hash->setAlgorithm(12);
            break;
          case "HMACSHA256":
            $hash->setAlgorithm(13);
            break;
          case "HMACSHA384":
            $hash->setAlgorithm(14);
            break;
          case "HMACSHA512":
            $hash->setAlgorithm(15);
            break;
          case "HMACRIPEMD160":
            $hash->setAlgorithm(16);
            break;
          case "SHA3-224":
            $hash->setAlgorithm(17);
            break;
          case "SHA3-256":
            $hash->setAlgorithm(18);
            break;
          case "SHA3-384":
            $hash->setAlgorithm(19);
            break;
          case "SHA3-512":
            $hash->setAlgorithm(20);
            break;
          case "SHA512-224":
            $hash->setAlgorithm(21);
            break;
          case "SHA512-256":
            $hash->setAlgorithm(22);
            break;
          default:
            echo "Invalid algorithm selection.\n";
            return;
        }
      }
      if ($argv[$i] == "-f") {
        $hash->setInputFile($argv[$i + 1]);
      }
      if ($argv[$i] == "-s") {
        $hash->setInputMessage($argv[$i + 1]);
      }
      if ($argv[$i] == "-hex") {
        $hash->setEncodeHash(true);
      }
    }
  }

  $hash->doComputeHash();
  echo "Hash complete! Hash value: " . $hash->getHashValue() . "\n";

}  catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>