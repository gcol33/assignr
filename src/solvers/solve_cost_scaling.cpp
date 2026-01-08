// src/solve_cost_scaling.cpp
// Gabow-Tarjan cost-scaling algorithm for LAP (exact)

#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>
#include "../core/lap_utils.h"

using namespace Rcpp;

// Implementation of Gabow-Tarjan cost-scaling
Rcpp::List solve_cost_scaling_impl(NumericMatrix cost, bool maximize) {
  const int n0 = cost.nrow();
  const int m0 = cost.ncol();
  
  if (n0 == 0 || m0 == 0) {
    return make_result(std::vector<int>(), 0.0);
  }

  bool transposed = false;
  NumericMatrix C = cost;
  int n = n0, m = m0;

  // Ensure rows <= cols (transpose if needed)
  if (n0 > m0) {
    C = transpose(cost);
    n = m0;
    m = n0;
    transposed = true;
  }

  // Find max finite value for maximize transformation
  double cmax = -std::numeric_limits<double>::infinity();
  bool has_finite = false;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (std::isfinite(v)) {
        has_finite = true;
        if (v > cmax) cmax = v;
      }
    }
  }
  
  if (!has_finite) {
    LAP_ERROR("No finite costs found.");
  }

  // Working costs: use BIG for forbidden edges; transform for maximize
  std::vector<std::vector<double>> W(n, std::vector<double>(m));
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (!std::isfinite(v)) {
        W[i][j] = BIG;
      } else {
        W[i][j] = maximize ? (cmax - v) : v;
      }
    }
  }

  // Dual variables (potentials)
  std::vector<double> u(n, 0.0);
  std::vector<double> v(m, 0.0);

  // Initialize column duals by column minima
  for (int j = 0; j < m; ++j) {
    double min_val = W[0][j];
    for (int i = 1; i < n; ++i) {
      if (W[i][j] < min_val) min_val = W[i][j];
    }
    v[j] = min_val;
  }

  // Matching arrays
  std::vector<int> row_match(n, -1);
  std::vector<int> col_match(m, -1);

  // Lambda for reduced cost
  auto red = [&](int i, int j) -> double {
    return W[i][j] - u[i] - v[j];
  };

  // Augment for each free row
  for (int root = 0; root < n; ++root) {
    if (row_match[root] != -1) continue;

    std::vector<bool> S(n, false);
    std::vector<bool> T(m, false);
    std::vector<int> parent_col(m, -1);
    S[root] = true;

    while (true) {
      bool progressed = false;

      // Scan zero-reduced edges from S to ~T
      for (int i = 0; i < n; ++i) {
        if (!S[i]) continue;
        
        for (int j = 0; j < m; ++j) {
          if (T[j]) continue;
          
          if (std::abs(red(i, j)) < TOL) {  // zero reduced cost
            parent_col[j] = i;
            T[j] = true;
            progressed = true;

            if (col_match[j] == -1) {
              // Augment along alternating path ending at column j
              int cj = j;
              while (true) {
                int ri = parent_col[cj];
                int pj = row_match[ri];
                row_match[ri] = cj;
                col_match[cj] = ri;
                if (ri == root) break;
                cj = pj;
              }
              break;
            } else {
              int i2 = col_match[j];
              if (!S[i2]) {
                S[i2] = true;
              }
            }
          }
        }
        
        if (row_match[root] != -1) break;
      }

      if (row_match[root] != -1) break;
      if (progressed) continue;

      // No zero edges found: relabel
      // delta = min reduced cost over i in S, j not in T
      double delta = std::numeric_limits<double>::infinity();
      for (int i = 0; i < n; ++i) {
        if (!S[i]) continue;
        for (int j = 0; j < m; ++j) {
          if (T[j]) continue;
          double r = red(i, j);
          if (r < delta) delta = r;
        }
      }

      if (!std::isfinite(delta) || delta >= BIG / 2.0) {
        LAP_ERROR("Infeasible: forbidden edges block all matchings.");
      }

      // Update duals: increase u on S, decrease v on T
      for (int i = 0; i < n; ++i) {
        if (S[i]) u[i] += delta;
      }
      for (int j = 0; j < m; ++j) {
        if (T[j]) v[j] -= delta;
      }
      // Continue: at least one new zero appears
    }
  }

  // Build output in original orientation
  std::vector<int> match_rows;
  double total = 0.0;

  if (!transposed) {
    match_rows = row_match;
    for (int i = 0; i < n; ++i) {
      int j = match_rows[i];
      if (j >= 0 && j < m0) {
        double val = cost(i, j);
        if (std::isfinite(val)) {
          total += val;  // Use original cost value directly
        }
      }
    }
  } else {
    // Transposed case: original was m0 x n0, we worked on n x m
    match_rows.assign(n0, -1);
    for (int i = 0; i < n; ++i) {
      int j = row_match[i];
      if (j >= 0 && j < m) {
        match_rows[j] = i;
        double val = cost(j, i);
        if (std::isfinite(val)) {
          total += val;  // Use original cost value directly
        }
      }
    }
  }

  // Convert to 1-based for R
  for (int &x : match_rows) {
    if (x >= 0) ++x;
  }

  return make_result(match_rows, total);
}
