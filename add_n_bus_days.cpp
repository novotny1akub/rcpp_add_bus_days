#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]


// https://stackoverflow.com/questions/65809643/r-recursive-function-to-add-n-days-to-a-date-skipping-saturdays-sundays-and-bank
// https://teuder.github.io/rcpp4everyone_en/180_date.html#member-functions-date
// https://www.codecademy.com/learn/paths/c-plus-plus-for-programmers/tracks/c-plus-plus-for-programmers/modules/getting-started-with-c-plus-plus/cheatsheet
// https://www.geeksforgeeks.org/how-to-calculate-the-easter-date-for-a-given-year-using-gauss-algorithm/


// [[Rcpp::export]]
Date rcpp_gauss_easter(int Y){
  
  float A, B, C, P, Q, M, N, D, E;
  Date dt_out;
  
  // All calculations done
  // on the basis of
  // Gauss Easter Algorithm
  A = Y % 19;
  B = Y % 4;
  C = Y % 7;
  P = (float)floor(Y / 100);
  Q = (float)floor((13 + 8 * P) / 25);
  M = (int)(15 - Q + P - P / 4) % 30;
  N = (int)(4 + P - P / 4) % 7;
  D = (int)(19 * A + M) % 30;
  E = (int)(2 * B + 4 * C + 6 * D + N) % 7;
  int days = (int)(22 + D + E);
  
  // A corner case,
  // when D is 29
  if ((D == 29) && (E == 6)) {
    dt_out = Date(std::to_string(Y) + "-04-19");
  }
  // Another corner case,
  // when D is 28
  else if ((D == 28) && (E == 6)) {
    dt_out = Date(std::to_string(Y) + "-04-18");
  }
  else {
    // If days > 31, move to April
    // April = 4th Month
    if (days > 31) {
      dt_out = Date(std::to_string(Y) + "-04-" + std::to_string(days - 31));
    }
    else {
      // Otherwise, stay on March
      // March = 3rd Month
      dt_out = Date(std::to_string(Y) + "-03-" + std::to_string(days));
    }
  }
  return(dt_out);
}

// [[Rcpp::export]]
DateVector rcpp_add_n_bus_days(DateVector v1, IntegerVector n_days) {
  // add one day if n_days_int greater than 0
  // check if saturday or sunday or bank holiday or easter (handled with rcpp_gauss_easter)
  // if not, return the date
  // if bank holiday, go on adding additional days

  DateVector v2(v1.length());
  IntegerVector n_days_vctr(v1.length());
  Date s1;
  bool is_h;
  int n_days_int;
  int sgn;
  
  if(n_days.length() == 1){
    n_days_int = n_days[0];
    n_days_vctr.fill(n_days_int);
  }
  else {
    n_days_vctr = n_days;
  }


  for (int i = 0; i < v1.length(); i++) {

    n_days_int = abs(n_days_vctr[i]);
    if (n_days_vctr[i] >= 0){
      sgn = 1;
    }
    else {
      sgn = -1;
    }
    s1 = v1[i];

    while (n_days_int > 0) {
      s1 = s1 + 1*sgn;
      n_days_int = n_days_int - 1;
      is_h =
        s1.getWeekday() == 1 or
        s1.getWeekday() == 7 or
        (s1.getDay() == 1 and s1.getMonth() == 1) or
        (s1.getDay() == 1 and s1.getMonth() == 5) or
        (s1.getDay() == 8 and s1.getMonth() == 5) or
        (s1.getDay() == 5 and s1.getMonth() == 7) or
        (s1.getDay() == 6 and s1.getMonth() == 7) or
        (s1.getDay() == 28 and s1.getMonth() == 9) or
        (s1.getDay() == 28 and s1.getMonth() == 10) or
        (s1.getDay() == 17 and s1.getMonth() == 11) or
        (s1.getDay() == 24 and s1.getMonth() == 12) or
        (s1.getDay() == 25 and s1.getMonth() == 12) or
        (s1.getDay() == 26 and s1.getMonth() == 12) or
        (rcpp_gauss_easter(s1.getYear()) + 1) == s1 or
        (rcpp_gauss_easter(s1.getYear()) + -2) == s1;

      if (is_h == true) {
        n_days_int = n_days_int + 1;
      }
      else {

      }

    }

    v2[i] = s1;
  }


  return(v2);
}


/*** R

n <- 2000
rcpp_add_n_bus_days(as.Date("2022-04-11") + 0:(n - 1), n_days = rep(1, n) )

*/
