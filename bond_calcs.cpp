#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' Function for calculating metrics for one bond
//[[Rcpp::export]] 
List cpp_get_bond_metrics(double ttm, double yield, double c, double FV = 100.0, int periodicity = 2) { 
  
  if (ttm <= 0)
    return List::create(); // Exits early if bond is expired
  
  int n = std::max(1, (int)std::ceil(ttm * periodicity)); // Calculate number of payment periods remaining (including partial periods)
  
  
  // Set up output vectors of length n (pre-allocates memory)
  NumericVector pmt_times(n);
  NumericVector cashflows(n);
  NumericVector pv_cashflows(n);
  
  // Setting up variables for sum calcs
  double bond_price = 0;
  double mac_duration_sum = 0;
  double convexity_sum = 0;
  
  // Initial calculations
  double c_pmt = (FV * c) / periodicity; // Calculate coupon payment
  double r_periodic = yield / periodicity; // Calculate periodic rate
  double dt = 1.0 / periodicity; // Calculate time between payments
  
  // Loop for per cashflow calculations 
  for(int i = 0; i < n; ++i) {
    pmt_times[i] = (i + 1) * (1.0 / periodicity); // Add this payment time to vector
    if (i == n - 1 || pmt_times[i] > ttm) {
      pmt_times[i] = ttm; // Set last payment time to expiration date
    }
    
    cashflows[i] = c_pmt; // Add this notional cashflow to vector
    if (i == n - 1)
      cashflows[i] += FV; // Set final payment to coupon + bond redemption
    
    pv_cashflows[i] = cashflows[i] / std::pow(1.0 + r_periodic, periodicity * pmt_times[i]); // Add PV of this cashflow to vector
    
    bond_price += pv_cashflows[i]; // Summation of cashflow PVs = bond price
    mac_duration_sum += pmt_times[i] * pv_cashflows[i];  // Summation for Macaulay Duration numerator
    convexity_sum += pmt_times[i] * (pmt_times[i] + dt) * pv_cashflows[i]; // Summation for Convexity numerator
  }
  
  // Duration and Convexity
  double macaulay_duration = mac_duration_sum / bond_price; // Calculate Macaulay duration
  double modified_duration = macaulay_duration / (1.0 + r_periodic); // Calculate Modified Duration
  double convexity = convexity_sum / (bond_price * std::pow(1.0 + r_periodic, 2)); // Calculate Convexity
  
  
  // Recalculating bond price at different yields
  auto calc_bond_price = [&](double y) { 
    double p = 0;
    double rp = y / periodicity;
    for(int i = 0; i < n; ++i) { // Loop through each cashflow for given yield
      p += cashflows[i] / std::pow(1.0 + rp, periodicity * pmt_times[i]); // Calculates PV of this cashflow and adds to price
    }
    return p; // returns bond price at given yield
  };
  
  // Delta and Gamma
  double step_size = 0.01; // 1 bp step size
  
  double price_plus = calc_bond_price(yield + step_size); // Calculates bond price with a 1bp yield increase
  double price_minus = calc_bond_price(yield - step_size); // Calculates bond price with a 1bp yield decrease
  
  double delta_central_approx = (price_plus - price_minus) / (2 * step_size) / 10000.0; // Delta Central Approximation
  double delta_forward_approx = (price_plus - bond_price) / step_size / 10000.0;
  double gamma_approx = 0.5 * ((price_plus - 2 * bond_price + price_minus) / std::pow(step_size, 2)) / std::pow(10000.0, 2);
  
  return List::create(
    Named("n_pmt_periods") = n,
    Named("c_pmt") = c_pmt,
    Named("cashflows") = cashflows,
    Named("pv_cashflows") = pv_cashflows,
    Named("bond_price") = bond_price,
    Named("macaulay_duration") = macaulay_duration,
    Named("modified_duration") = modified_duration,
    Named("convexity") = convexity,
    Named("price_plus") = price_plus,
    Named("price_minus") = price_minus,
    Named("delta_central_approx") = delta_central_approx,
    Named("delta_forward_approx") = delta_forward_approx,
    Named("gamma_approx") = gamma_approx
  );
}




//' Function for calculating metrics for a dataframe of bonds
// [[Rcpp::export]] 
DataFrame cpp_get_portfolio_metrics(NumericVector ttm, NumericVector yield, NumericVector c, 
                                    NumericVector FV = NumericVector::create(100.0), 
                                    IntegerVector periodicity = IntegerVector::create(2)) {
  
  int n_bonds = ttm.size(); // Number of bonds to calculate metrics for
  
  // Set up output vectors of length n_bonds (pre-allocates memory)
  NumericVector price(n_bonds);
  NumericVector macaulay_duration(n_bonds);
  NumericVector modified_duration(n_bonds);
  NumericVector convexity(n_bonds);
  NumericVector delta_central_approx(n_bonds);
  NumericVector gamma_approx(n_bonds);
  
  // Loops through each bond in df
  for(int i = 0; i < n_bonds; ++i) {
    
    // Checks whether FV is entered as a constant (as is default) or differs between bonds
    double current_fv; 
    if (FV.size() == 1) { 
      current_fv = FV[0]; // if FV is the same for all bonds, use that number (doesn't check each row)
    } else {
      current_fv = FV[i]; // if FV is different between bonds, use each rows FV
    }
    
    // Check whether periodicity is entered as a constant (as is default) or differs between bonds
    double current_periodicity; 
    if (periodicity.size() == 1) { 
      current_periodicity = periodicity[0]; // if periodicity is constant, use that number (doesn't check each row)
    } else {
      current_periodicity = periodicity[i]; // if periodicity is different between bonds, use each rows periodicity
    }
    
    // Calls bond metric calculator function for this bond
    List output = cpp_get_bond_metrics(ttm[i], yield[i], c[i], current_fv, current_periodicity);
    
    // Extract Results
    if (output.size() > 0) {
      price[i] = output["bond_price"];
      macaulay_duration[i] = output["macaulay_duration"];
      modified_duration[i] = output["modified_duration"];
      convexity[i] = output["convexity"];
      delta_central_approx[i] = output["delta_central_approx"];
      gamma_approx[i] = output["gamma_approx"];
    } else{                                 // if list is empty (ie bond is expired) set to NA
      price[i] = NA_REAL;
      macaulay_duration[i] = NA_REAL;
      modified_duration[i] = NA_REAL;
      convexity[i] = NA_REAL;
      delta_central_approx[i] = NA_REAL;
      gamma_approx[i] = NA_REAL;
    }
  }
  
  return DataFrame::create( // Outputs as df
    Named("bond_price") = price,
    Named("macaulay_duration") = macaulay_duration,
    Named("modified_duration") = modified_duration,
    Named("convexity") = convexity,
    Named("delta_central_approx") = delta_central_approx,
    Named("gamma_approx") = gamma_approx
    );
}


//' Function for creating df of bond payments
// [[Rcpp::export]]
DataFrame cpp_get_portfolio_cfs(DateVector start_dates, DateVector end_dates, 
                                NumericVector coupons, NumericVector periodicities, 
                                NumericVector face_values, NumericVector quantities) {
  
  // Setting up dynamic length output vectors
  std::vector<Date> output_dates; 
  std::vector<double> output_cfs;
  std::vector<int> output_bond_ids;
  
  int n_bonds = start_dates.size(); // number of bonds to get cashflows for
  
  // Loop through each bond
  for (int i = 0; i < n_bonds; ++i) {
    
    // Initial Calculations
    double coupon_pmt = (coupons[i] / periodicities[i]) * face_values[i] * quantities[i]; // Calc coupon payment
    double final_pmt = coupon_pmt + (face_values[i] * quantities[i]); // Calc final payment (coupon + FV)
    
    // int months_between = 12 / periodicities[i]; 
    int days_between = std::round(365.25 / periodicities[i]); 
    // Note: months_between is unused rn because we are just adding approx dates, but keeping it
    // for when I figure out how to add actual exact dates
  
    // Initialize Date counters
    Date current_date = start_dates[i];
    Date end_date = end_dates[i];
    
    // Loop through each payment date
    while (current_date <= end_date) {
      output_bond_ids.push_back(i + 1); // Adds a new row with the id for this bond
      output_dates.push_back(current_date); // Adds a new row with this payment date
      
      if (current_date == end_date) {
        output_cfs.push_back(final_pmt); // if this is the last payment date, add row with final payment
      } else {
        output_cfs.push_back(coupon_pmt); // if not, add new row with coupon pmt
      }
      
      // Increment date 
      current_date = current_date + days_between; 
      
      // Checks to make sure we didn't skip over the end date bc of rounding
      if (current_date > end_date && output_dates.back() < end_date) {
        current_date = end_date; // if we did, set payment date back to end date
      }
    }
  }
  // Returns as long format df (wrap() converts them from std::vectors to vectors that can be read by R)
  return DataFrame::create(
    Named("bond_id") = wrap(output_bond_ids),
    Named("date") = wrap(output_dates),
    Named("cf") = wrap(output_cfs)
  );
}
  
  
  
  
  