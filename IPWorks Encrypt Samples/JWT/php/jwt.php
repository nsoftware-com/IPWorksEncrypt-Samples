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
require_once('../include/ipworksencrypt_jwt.php');
require_once('../include/ipworksencrypt_certmgr.php');
require_once('../include/ipworksencrypt_const.php');
?>
<?php
$signedText = "";
$algorithm = "HS256";
$keyText = "txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=";
$claimAudience = "test";
$claimSubject = "test";
$claimIssuer = "test";
$claimExp = "12012017";
$jwt = new IPWorksEncrypt_Jwt();

try{

if($_SERVER['REQUEST_METHOD'] == "POST") {
    $signedText = "";
    $algorithm = $_POST["algorithm"];
    $signedText = $_POST["signed"];
    $claimAudience = $_POST["aud"];
    $claimSubject = $_POST["sub"];
    $claimIssuer = $_POST["iss"];
	$claimExp = $_POST["exp"];
    if ($algorithm == "HS256") {
        $keyText = "txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=";
    } else if ($algorithm == "HS384") {
        $keyText = "5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2";
    } else if ($algorithm == "HS512") {
        $keyText = "AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==";
    } 
    
    if (isset($_POST["sign"])) {
    if ($algorithm == "HS256") {
        $jwt->setSigningAlgorithm(0);
    } else if ($algorithm == "HS384") {
        $jwt->setSigningAlgorithm(1);
    } else if ($algorithm == "HS512") {
        $jwt->setSigningAlgorithm(2);
    }
	$jwt->doConfig("KeyEncoding=1"); //base64
	$jwt->setKey($keyText);
    $jwt->setClaimAudience($claimAudience);
    $jwt->setClaimSubject($claimSubject);
    $jwt->setClaimIssuer($claimIssuer);
	$jwt->setClaimExp($claimExp);
	


	$jwt->doSign();
	$signedText = $jwt->getEncodedJWT();
    $claimAudience ="";
	$claimSubject = "";
	$claimIssuer = "";
	$claimExp = "";
}
 if (isset($_POST["verify"])) {
	if ($algorithm == "HS256") {
		$jwt->setSigningAlgorithm(0);
	} else if ($algorithm == "HS384") {
		$jwt->setSigningAlgorithm(1);		
	} else if ($algorithm == "HS512") {
		$jwt->setSigningAlgorithm(2);
	}
	$jwt->doConfig("KeyEncoding=1"); //base64
    $jwt->setKey($keyText);
    $jwt->setEncodedJWT($signedText);
	$jwt->doVerify();
    $signedText = "";
	$claimAudience =$jwt->getClaimAudience();
	$claimSubject = $jwt->getClaimSubject();
	$claimIssuer = $jwt->getClaimIssuer();
	$claimExp = $jwt->getClaimExp();
}
  
}
  } catch (Exception $e) {
    echo '<font color="red">Error: ',  $e->getMessage(), "</font><br/>";
  }
?>

<form method=POST name=formjwt>
<center>
<table width="90%">
  <tr>
    <td>
	  Algorithm:
	  <p>	
	  <select name="algorithm" onChange="formjwt.submit();">
        <option value="HS256" <?php echo ($algorithm=="HS256")?"selected":""?>>HS256</option>
        <option value="HS384" <?php echo ($algorithm=="HS384")?"selected":""?>>HS384</option>
	    <option value="HS512" <?php echo ($algorithm=="HS512")?"selected":""?>>HS512</option>
      </select>
    </td>
	<td>Key:</br><input type="text" name="key" size="50" value="<?php echo $keyText; ?>">
  </tr>
  
  <tr>
	<td>Claims:</td>
	<td>Signed JWT:</td>
  </tr>
					
  <tr>
	  <td>
	  <p style="width:50%;">
	  Claim Audience:<input type="text" name="aud" size="50" value="<?php echo $claimAudience; ?>">
	  <p style="width:50%;">
	  Claim Subject:<input type="text" name="sub" size="50" value="<?php echo $claimSubject; ?>">
	  <p style="width:50%;">
	  Claim Issuer:<input type="text" name="iss" size="50" value="<?php echo $claimIssuer; ?>">
	  <p style="width:50%;">
	  Claim Expiration:<input type="text" name="exp" size="50" value="<?php echo $claimExp; ?>">
	  </td>
	  <td><textarea name="signed" cols="55" rows="15"><?php echo $signedText; ?></textarea></td>
  </tr>
  <tr>
    <td>
	<input type="submit" name="sign" value="Sign >>>">
    </td>
	<td>
	<input type="submit" name="verify" value="<<< Verify">
	</td>
  </tr>

</table>
</center>
</form>