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
require_once('../include/ipworksencrypt_ezrand.php');
require_once('../include/ipworksencrypt_const.php');
?>
<?php

if ($argc < 3) {
  echo "usage: php ezrand.php [-i -min min -max max] [-c count] [-l length] [-s seed] -alg algorithm\n\n";
  echo "  -i           whether to generate random integers instead of bytes (optional)\n";
  echo "  -min         the lower bound of the random number to be generated (inclusive) (optional, default 0)\n";
  echo "  -max         the upper bound of the random number to be generated (exclusive) (optional, default 100)\n";
  echo "  -c           the number of random integers or byte arrays to generate\n";
  echo "  -l           the length of the byte array to be generated (optional, default 16)\n";
  echo "  -s           the seed to use (optional)\n";
  echo "  -alg         the random number algorithm to use, chosen from {ISAAC, CryptoAPI, Platform, RC4Random}\n";
  echo "\nExample: php ezrand.php -i -min 0 -max 1000 -c 5 -alg ISAAC\n";
  return;
}

try{
  $ezrand = new IPWorksEncrypt_Ezrand();
  $count = 1;
  $ints = false;

  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-alg") {
        switch (strtolower($argv[$i + 1])) {
          case "isaac":
            $ezrand->setAlgorithm(0);
            break;
          case "cryptoapi":
            $ezrand->setAlgorithm(1);
            break;
          case "platform":
            $ezrand->setAlgorithm(2);
            break;
          case "rc4random":
            $ezrand->setAlgorithm(4);
            break;
          default:
            echo "Invalid algorithm selection.\n";
            return;
        }
      }
      if ($argv[$i] == "-c") {
        $count = $argv[$i + 1];
      }
      if ($argv[$i] == "-min") {
        $ezrand->setMin($argv[$i + 1]);
      }
      if ($argv[$i] == "-max") {
        $ezrand->setMax($argv[$i + 1]);
      }
      if ($argv[$i] == "-s") {
        $ezrand->setSeed($argv[$i + 1]);
      }
      if ($argv[$i] == "-l") {
        $ezrand->setRandBytesLength($argv[$i + 1]);
      }
      if ($argv[$i] == "-i") {
        $ints = true;
      }
    }
  }

  if ($ints) {
    for ($i = 0; $i < $count; $i++) {
      $ezrand->doGetNextInt();
      echo $ezrand->GetRandInt() . "\n";
    }
  } else {
    $ezrand->doConfig("OutputEncoding=2"); // hex
    for ($i = 0; $i < $count; $i++) {
      $ezrand->doGetNextBytes();
      echo $ezrand->GetRandBytes() . "\n";
    }
  }
}  catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>