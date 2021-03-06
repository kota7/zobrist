// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// ZobristHash
int ZobristHash(IntegerVector& key, std::vector<unsigned int>& randomint);
RcppExport SEXP zobrist_ZobristHash(SEXP keySEXP, SEXP randomintSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector& >::type key(keySEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    rcpp_result_gen = Rcpp::wrap(ZobristHash(key, randomint));
    return rcpp_result_gen;
END_RCPP
}
// ZobristHash_vec
IntegerVector ZobristHash_vec(std::vector< std::vector<int> >& keys, std::vector<unsigned int>& randomint);
RcppExport SEXP zobrist_ZobristHash_vec(SEXP keysSEXP, SEXP randomintSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> >& >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    rcpp_result_gen = Rcpp::wrap(ZobristHash_vec(keys, randomint));
    return rcpp_result_gen;
END_RCPP
}
// LocateKey
IntegerVector LocateKey(IntegerVector& key, int keysize, std::vector<unsigned int>& randomint, List& hashtable);
RcppExport SEXP zobrist_LocateKey(SEXP keySEXP, SEXP keysizeSEXP, SEXP randomintSEXP, SEXP hashtableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector& >::type key(keySEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    Rcpp::traits::input_parameter< List& >::type hashtable(hashtableSEXP);
    rcpp_result_gen = Rcpp::wrap(LocateKey(key, keysize, randomint, hashtable));
    return rcpp_result_gen;
END_RCPP
}
// LocateKeys
IntegerMatrix LocateKeys(ListOf<IntegerVector>& keys, int keysize, std::vector<unsigned int>& randomint, List& hashtable);
RcppExport SEXP zobrist_LocateKeys(SEXP keysSEXP, SEXP keysizeSEXP, SEXP randomintSEXP, SEXP hashtableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf<IntegerVector>& >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    Rcpp::traits::input_parameter< List& >::type hashtable(hashtableSEXP);
    rcpp_result_gen = Rcpp::wrap(LocateKeys(keys, keysize, randomint, hashtable));
    return rcpp_result_gen;
END_RCPP
}
// GetValueByKey
List GetValueByKey(IntegerVector& key, int keysize, std::vector<unsigned int>& randomint, List& hashtable);
RcppExport SEXP zobrist_GetValueByKey(SEXP keySEXP, SEXP keysizeSEXP, SEXP randomintSEXP, SEXP hashtableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector& >::type key(keySEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    Rcpp::traits::input_parameter< List& >::type hashtable(hashtableSEXP);
    rcpp_result_gen = Rcpp::wrap(GetValueByKey(key, keysize, randomint, hashtable));
    return rcpp_result_gen;
END_RCPP
}
// GetValueByKeys
List GetValueByKeys(ListOf<IntegerVector>& keys, int keysize, std::vector<unsigned int>& randomint, List& hashtable);
RcppExport SEXP zobrist_GetValueByKeys(SEXP keysSEXP, SEXP keysizeSEXP, SEXP randomintSEXP, SEXP hashtableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf<IntegerVector>& >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    Rcpp::traits::input_parameter< List& >::type hashtable(hashtableSEXP);
    rcpp_result_gen = Rcpp::wrap(GetValueByKeys(keys, keysize, randomint, hashtable));
    return rcpp_result_gen;
END_RCPP
}
// FindKey
bool FindKey(IntegerVector& key, int keysize, std::vector<unsigned int>& randomint, List& hashtable);
RcppExport SEXP zobrist_FindKey(SEXP keySEXP, SEXP keysizeSEXP, SEXP randomintSEXP, SEXP hashtableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector& >::type key(keySEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    Rcpp::traits::input_parameter< List& >::type hashtable(hashtableSEXP);
    rcpp_result_gen = Rcpp::wrap(FindKey(key, keysize, randomint, hashtable));
    return rcpp_result_gen;
END_RCPP
}
// FindKeys
LogicalVector FindKeys(ListOf<IntegerVector>& keys, int keysize, std::vector<unsigned int>& randomint, List& hashtable);
RcppExport SEXP zobrist_FindKeys(SEXP keysSEXP, SEXP keysizeSEXP, SEXP randomintSEXP, SEXP hashtableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf<IntegerVector>& >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    Rcpp::traits::input_parameter< List& >::type hashtable(hashtableSEXP);
    rcpp_result_gen = Rcpp::wrap(FindKeys(keys, keysize, randomint, hashtable));
    return rcpp_result_gen;
END_RCPP
}
// KeyToStr
std::string KeyToStr(IntegerVector x, int keysize);
RcppExport SEXP zobrist_KeyToStr(SEXP xSEXP, SEXP keysizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    rcpp_result_gen = Rcpp::wrap(KeyToStr(x, keysize));
    return rcpp_result_gen;
END_RCPP
}
// StrToKey
IntegerVector StrToKey(std::string x, int keysize);
RcppExport SEXP zobrist_StrToKey(SEXP xSEXP, SEXP keysizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    rcpp_result_gen = Rcpp::wrap(StrToKey(x, keysize));
    return rcpp_result_gen;
END_RCPP
}
// KeysToStrs
CharacterVector KeysToStrs(ListOf<IntegerVector> x, int keysize);
RcppExport SEXP zobrist_KeysToStrs(SEXP xSEXP, SEXP keysizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf<IntegerVector> >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    rcpp_result_gen = Rcpp::wrap(KeysToStrs(x, keysize));
    return rcpp_result_gen;
END_RCPP
}
// StrsToKeys
ListOf<IntegerVector> StrsToKeys(CharacterVector x, int keysize);
RcppExport SEXP zobrist_StrsToKeys(SEXP xSEXP, SEXP keysizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    rcpp_result_gen = Rcpp::wrap(StrsToKeys(x, keysize));
    return rcpp_result_gen;
END_RCPP
}
// MakeHashTable
List MakeHashTable(int htsize, List values, IntegerVector index);
RcppExport SEXP zobrist_MakeHashTable(SEXP htsizeSEXP, SEXP valuesSEXP, SEXP indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type htsize(htsizeSEXP);
    Rcpp::traits::input_parameter< List >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type index(indexSEXP);
    rcpp_result_gen = Rcpp::wrap(MakeHashTable(htsize, values, index));
    return rcpp_result_gen;
END_RCPP
}
// RemakeHashTable
List RemakeHashTable(List& oldtable, int htsize, int keysize, std::vector<unsigned int>& randomint);
RcppExport SEXP zobrist_RemakeHashTable(SEXP oldtableSEXP, SEXP htsizeSEXP, SEXP keysizeSEXP, SEXP randomintSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type oldtable(oldtableSEXP);
    Rcpp::traits::input_parameter< int >::type htsize(htsizeSEXP);
    Rcpp::traits::input_parameter< int >::type keysize(keysizeSEXP);
    Rcpp::traits::input_parameter< std::vector<unsigned int>& >::type randomint(randomintSEXP);
    rcpp_result_gen = Rcpp::wrap(RemakeHashTable(oldtable, htsize, keysize, randomint));
    return rcpp_result_gen;
END_RCPP
}
