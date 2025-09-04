/*
 * IPWorks Encrypt 2024 Java Edition - Sample Project
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

import java.io.*;
import ipworksencrypt.*;

public class ezrand extends ConsoleDemo {

  public static void main(String[] args) {


    try {
      int algorithm = 1;
      String seed = "";
      EzRand ezrand1 = new EzRand();

      String newline = System.getProperty("line.separator");

      System.out.println("*****************************************************************************");
      System.out.println("* This demo shows how to generate random numbers and byte arrays using      *");
      System.out.println("* various algorithms such as ISAAC.                                         *");
      System.out.println("*****************************************************************************");

      while (true) {
        System.out.println(newline + "Specify an algorithm:");
        System.out.println("1) ISAAC");
        System.out.println("2) Platform");
        System.out.println("3) Secure Platform");

        algorithm = Integer.parseInt(prompt(newline + "Algorithm",":","" + algorithm));
        switch (algorithm) {
        case 1:
          ezrand1.setAlgorithm(EzRand.raISAAC);
          break;
        case 2:
          ezrand1.setAlgorithm(EzRand.raPlatform);
          break;
        case 3:
          ezrand1.setAlgorithm(EzRand.raSecurePlatform);
          break;
        default:
          System.out.println("Invalid algorithm.");
          continue;
        }

        char option = ask(newline + "Get random Integers(i) or Bytes(b)", "?", "(i/b)");
        if (option == 'i') {
          ezrand1.setMin(Integer.parseInt(prompt(newline + "Min Value", ":", "" + ezrand1.getMin())));
          ezrand1.setMax(Integer.parseInt(prompt("Max Value", ":", "" + ezrand1.getMax())));
          seed = prompt("Seed", ":", seed);
          ezrand1.setSeed(seed);

          System.out.println("Getting random integers...");
          System.out.println("Press <enter> to get next int or q to quit");
          do {
            ezrand1.getNextInt();
            System.out.println(ezrand1.getRandInt());
          } while (!input().trim().equalsIgnoreCase("q"));
        } else if (option == 'b') {
          ezrand1.setRandBytesLength(Integer.parseInt(prompt(newline + "Bytes Length", ":", "" + ezrand1.getRandBytesLength())));
          seed = prompt("Seed", ":", seed);
          ezrand1.setSeed(seed);

          System.out.println("Getting " + ezrand1.getRandBytesLength() + " random bytes...");
          System.out.println("Press <enter> to get next bytes or q to quit");
          do {
            ezrand1.getNextBytes();
            System.out.println(displayBytes(ezrand1.getRandBytes()));
          } while (!input().trim().equalsIgnoreCase("q"));
        } else {
          System.out.println("Invalid option.");
          continue;
        }

        if (ask(newline + "Do you wish to generate another set of random integers or bytes") != 'y')
          break;
      }

    } catch (Exception ex) {
      displayError(ex);
    }
  }

  private static String displayBytes(byte[] bytes) {
    String hexString = "";
    for (int i = 0; i < bytes.length; i++) {
      hexString += String.format("%02X ", bytes[i]);
    }
    hexString.trim();
    return hexString;
  }
}


class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksEncryptException) {
      System.out.print(" (" + ((IPWorksEncryptException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



