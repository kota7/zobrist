#include <Rcpp.h>
#include <string>
#include <map>
#include "keyStrConversion.h"
using namespace Rcpp;

// Naming rule of the hash table entries


// look up tables bin -> hex, and hex -> bin
char bin_to_hex(const std::string bin)
{
  if (bin == "0000") return '0';
  if (bin == "1000") return '1';
  if (bin == "0100") return '2';
  if (bin == "1100") return '3';
  if (bin == "0010") return '4';
  if (bin == "1010") return '5';
  if (bin == "0110") return '6';
  if (bin == "1110") return '7';
  if (bin == "0001") return '8';
  if (bin == "1001") return '9';
  if (bin == "0101") return 'A';
  if (bin == "1101") return 'B';
  if (bin == "0011") return 'C';
  if (bin == "1011") return 'D';
  if (bin == "0111") return 'E';
  if (bin == "1111") return 'F';

  // error, input is not binary of length 4
  stop("expecing binary string of length 4, but given: " + bin);
  return ' ';
}


std::string hex_to_bin(const char hex)
{
  switch(toupper(hex))
  {
    case '0': return "0000";
    case '1': return "1000";
    case '2': return "0100";
    case '3': return "1100";
    case '4': return "0010";
    case '5': return "1010";
    case '6': return "0110";
    case '7': return "1110";
    case '8': return "0001";
    case '9': return "1001";
    case 'A': return "0101";
    case 'B': return "1101";
    case 'C': return "0011";
    case 'D': return "1011";
    case 'E': return "0111";
    case 'F': return "1111";
  }
  std::string mess = "expecting hex letter, but given: ";
  mess.push_back(hex);
  stop(mess);
  return "    ";

}


std::string KeyToStr(IntegerVector x, int keysize)
{
  // Converts a key represented by integer vector to
  // string that represents the key
  // This is the inverse function of StrToKey()
  //
  // x       : IntegerVector one-based indices for positions of ones
  // keysize : bit size of key
  //
  //
  //
  // Returns:
  //   A string that represents the input
  //   Currently they are represented by a hex string in a reverse order
  //
  // Example:
  //   x = c(2, 4, 5)
  //   -> "01011" in reverse binary form
  //   -> "0101 1"
  //   -> "A1"    in hex form

  // input validation
  // check if 1 <= x <= keysize
  for (int i = 0; i < x.size(); i++)
  {
    if (x[i] < 1 || x[i] > keysize) {
      warning("index is out of bounds is ignored");
    }
  }


  int outsize = (keysize + 3) / 4;

  if (x.size() == 0) return std::string(outsize, '0');


  // determine the binary string size
  // first, find the maximum index in x, then
  // truncate by keysize
  int n = max(x);
  if (n > keysize) n = keysize;
  n = n + 3 - ((n-1) % 4); // this is to make sure n is a multiple of 4
  std::string bin(n, '0');
  for (int i = 0; i <  x.size(); i++)
    bin[x[i] - 1] = bin[x[i] - 1] == '0' ? '1':'0';


  std::string out(outsize, '0');
  for (unsigned int j = 0; j < out.size(); j++)
  {
    if (j*4 >= bin.size()) break;
    out[j] = bin_to_hex(bin.substr(j*4, 4));
  }

  return out;
}


IntegerVector StrToKey(std::string x, int keysize)
{
  // Converts a string to an integer vector that the key represents
  // This is the inverse function of KeyToStr()
  //
  // x       : string consisting of digits and a-f
  // keysize : maximum index allowed
  //
  // Returns:
  //   IntegerVector the corresponds to the string
  //
  // Example:
  //   x = "A1"
  //   -> "0101 1000" in reverse binary
  //   -> c(2, 4, 5)

  int n = x.size();
  std::string bin(n*4, ' ');
  for (int i = 0; i < n; i++)
    bin.replace(i*4, 4, hex_to_bin(x[i]));
  // discard redundant entries
  bin.resize(keysize);

  IntegerVector out;
  for (size_t i = 0; i < bin.size(); i++)
    if (bin[i] == '1') out.push_back(i+1);

  return out;
}




CharacterVector KeysToStrs(ListOf<IntegerVector> x, int keysize)
{
  int n = x.size();
  CharacterVector out(n);
  for (int i = 0; i < n; i++)
    out[i] = KeyToStr(x[i], keysize);
  return out;
}

ListOf<IntegerVector> StrsToKeys(CharacterVector x, int keysize)
{
  int n = x.size();
  //ListOf<IntegerVector> out;
  List out;
  for (int i = 0; i < n; i++)
    out.push_back(StrToKey(as<std::string>(x[i]), keysize));
    //out[i] = StrToKey(as<std::string>(x[i]));

  return out;
}


