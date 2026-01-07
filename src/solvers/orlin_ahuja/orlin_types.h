// src/solvers/orlin_ahuja/orlin_types.h
// Type definitions and constants for Orlin-Ahuja algorithm

#ifndef ORLIN_TYPES_H
#define ORLIN_TYPES_H

#include <vector>
#include <limits>
#include <cmath>

namespace orlin {

// Use double for costs/prices to handle scaling
using Cost = double;
using Price = double;

constexpr Cost INF_COST = std::numeric_limits<double>::infinity();
constexpr int UNASSIGNED = -1;

// Result of finding best/second-best column for a row
struct BestColumns {
    int best_col;           // column with highest profit
    int second_best_col;    // column with second highest profit
    Price best_profit;      // profit of best column
    Price second_profit;    // profit of second best (or -INF if none)

    // Bid amount = (best - second) + epsilon
    // This is always non-negative since best >= second
    Price bid_amount(Price epsilon) const {
        if (second_best_col == UNASSIGNED) {
            // Only one feasible column - bid just epsilon
            // (bid is always positive increment)
            return epsilon;
        }
        // best_profit >= second_profit by construction, so this is >= epsilon
        return (best_profit - second_profit) + epsilon;
    }
};

// Statistics for complexity verification
struct ScaleStats {
    int scale_number;
    Price epsilon;
    int augmentations;      // number of augmenting paths this scale
    int auction_rounds;     // number of auction rounds this scale
    int edges_scanned;      // total edges examined
    double time_ms;         // time for this scale
};

struct AlgorithmStats {
    std::vector<ScaleStats> scales;
    int total_augmentations;
    int total_scales;
    double total_time_ms;

    // For complexity verification
    double avg_augments_per_scale() const {
        if (scales.empty()) return 0;
        return static_cast<double>(total_augmentations) / scales.size();
    }
};

// Matching state
struct MatchingState {
    int n;                          // number of rows
    int m;                          // number of columns
    std::vector<int> row_to_col;    // row i assigned to column row_to_col[i], or UNASSIGNED
    std::vector<int> col_to_row;    // column j assigned to row col_to_row[j], or UNASSIGNED
    std::vector<Price> row_price;   // dual variable u[i]
    std::vector<Price> col_price;   // dual variable p[j] (the "prices")

    MatchingState(int n_, int m_)
        : n(n_), m(m_),
          row_to_col(n_, UNASSIGNED),
          col_to_row(m_, UNASSIGNED),
          row_price(n_, 0.0),
          col_price(m_, 0.0) {}

    int matching_size() const {
        int count = 0;
        for (int i = 0; i < n; ++i) {
            if (row_to_col[i] != UNASSIGNED) ++count;
        }
        return count;
    }

    bool is_complete() const {
        return matching_size() == n;
    }

    // Assign row i to column j, handling any displacement
    void assign(int i, int j) {
        // If column j was assigned to another row, unassign it
        if (col_to_row[j] != UNASSIGNED) {
            int old_row = col_to_row[j];
            row_to_col[old_row] = UNASSIGNED;
        }
        // If row i was assigned to another column, unassign it
        if (row_to_col[i] != UNASSIGNED) {
            int old_col = row_to_col[i];
            col_to_row[old_col] = UNASSIGNED;
        }
        row_to_col[i] = j;
        col_to_row[j] = i;
    }

    void unassign_row(int i) {
        if (row_to_col[i] != UNASSIGNED) {
            col_to_row[row_to_col[i]] = UNASSIGNED;
            row_to_col[i] = UNASSIGNED;
        }
    }
};

} // namespace orlin

#endif // ORLIN_TYPES_H
