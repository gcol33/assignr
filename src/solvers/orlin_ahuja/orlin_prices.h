// src/solvers/orlin_ahuja/orlin_prices.h
// Component 1: Price/Potential Management
// Theoretical bound: O(1) per update, O(m) for all reduced costs

#ifndef ORLIN_PRICES_H
#define ORLIN_PRICES_H

#include "orlin_types.h"
#include <vector>
#include <cmath>

namespace orlin {

// Compute reduced cost for edge (i, j)
// rc[i,j] = cost[i,j] - u[i] - p[j]
// For MINIMIZATION: feasible dual has rc >= 0
inline Price reduced_cost(
    const std::vector<Cost>& cost,  // row-major n x m
    int n, int m,
    int i, int j,
    const std::vector<Price>& row_price,
    const std::vector<Price>& col_price
) {
    Cost c = cost[i * m + j];
    if (!std::isfinite(c)) return INF_COST;
    return c - row_price[i] - col_price[j];
}

// Compute all reduced costs - O(m) operation
// Returns row-major matrix of reduced costs
inline std::vector<Price> compute_all_reduced_costs(
    const std::vector<Cost>& cost,
    int n, int m,
    const std::vector<Price>& row_price,
    const std::vector<Price>& col_price
) {
    std::vector<Price> rc(n * m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            rc[i * m + j] = reduced_cost(cost, n, m, i, j, row_price, col_price);
        }
    }
    return rc;
}

// Check epsilon-complementary slackness for the matching
// For assigned pair (i, j): rc[i,j] >= -epsilon (within tolerance)
// Returns true if all assigned pairs satisfy epsilon-CS
inline bool check_epsilon_cs(
    const std::vector<Cost>& cost,
    int n, int m,
    const MatchingState& state,
    Price epsilon,
    Price tolerance = 1e-9
) {
    for (int i = 0; i < n; ++i) {
        int j = state.row_to_col[i];
        if (j == UNASSIGNED) continue;

        Price rc = reduced_cost(cost, n, m, i, j, state.row_price, state.col_price);
        if (rc < -epsilon - tolerance) {
            return false;  // CS violation
        }
    }
    return true;
}

// Check dual feasibility: all reduced costs >= -epsilon
// This is a stronger condition than just checking assigned pairs
inline bool check_dual_feasibility(
    const std::vector<Cost>& cost,
    int n, int m,
    const std::vector<Price>& row_price,
    const std::vector<Price>& col_price,
    Price epsilon,
    Price tolerance = 1e-9
) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            Price rc = reduced_cost(cost, n, m, i, j, row_price, col_price);
            if (std::isfinite(rc) && rc < -epsilon - tolerance) {
                return false;
            }
        }
    }
    return true;
}

// Count how many edges violate epsilon-CS (have rc < -epsilon)
// Useful for debugging and complexity analysis
inline int count_cs_violations(
    const std::vector<Cost>& cost,
    int n, int m,
    const std::vector<Price>& row_price,
    const std::vector<Price>& col_price,
    Price epsilon
) {
    int violations = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            Price rc = reduced_cost(cost, n, m, i, j, row_price, col_price);
            if (std::isfinite(rc) && rc < -epsilon - 1e-9) {
                ++violations;
            }
        }
    }
    return violations;
}

// Verify optimality (for final solution): epsilon = 0
// 1. Matching is complete
// 2. All reduced costs >= 0 (dual feasibility)
// 3. Assigned pairs have rc = 0 (complementary slackness)
inline bool verify_optimality(
    const std::vector<Cost>& cost,
    int n, int m,
    const MatchingState& state,
    Price tolerance = 1e-9
) {
    // Check complete matching
    if (!state.is_complete()) return false;

    // Check dual feasibility and CS
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            Price rc = reduced_cost(cost, n, m, i, j, state.row_price, state.col_price);
            if (!std::isfinite(rc)) continue;

            // Dual feasibility: rc >= 0
            if (rc < -tolerance) return false;

            // Complementary slackness: if assigned, rc = 0
            if (state.row_to_col[i] == j) {
                if (std::abs(rc) > tolerance) return false;
            }
        }
    }
    return true;
}

// Compute total cost of current matching
inline Cost compute_matching_cost(
    const std::vector<Cost>& cost,
    int n, int m,
    const MatchingState& state
) {
    Cost total = 0;
    for (int i = 0; i < n; ++i) {
        int j = state.row_to_col[i];
        if (j != UNASSIGNED) {
            total += cost[i * m + j];
        }
    }
    return total;
}

} // namespace orlin

#endif // ORLIN_PRICES_H
