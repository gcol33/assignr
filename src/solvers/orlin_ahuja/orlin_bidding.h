// src/solvers/orlin_ahuja/orlin_bidding.h
// Component 2: Find Best/Second-Best Column (Bidding)
// Theoretical bound: O(degree(i)) per row, O(m) for all rows

#ifndef ORLIN_BIDDING_H
#define ORLIN_BIDDING_H

#include "orlin_types.h"
#include "orlin_prices.h"
#include <vector>
#include <cmath>

namespace orlin {

// Find the best and second-best columns for row i
// Profit = -cost[i,j] + col_price[j] (for minimization, we want min cost = max profit)
// Actually for min-cost assignment: profit = -reduced_cost = col_price[j] - cost[i,j] + row_price[i]
// But row_price[i] is constant for row i, so we just use: value[j] = col_price[j] - cost[i,j]
//
// Theoretical bound: O(m) for dense, O(degree(i)) for sparse
// Returns edges_scanned for complexity verification
inline BestColumns find_best_columns(
    const std::vector<Cost>& cost,  // row-major n x m
    int n, int m,
    int row,
    const std::vector<Price>& col_price,
    int* edges_scanned = nullptr
) {
    BestColumns result;
    result.best_col = UNASSIGNED;
    result.second_best_col = UNASSIGNED;
    result.best_profit = -INF_COST;
    result.second_profit = -INF_COST;

    int scanned = 0;

    for (int j = 0; j < m; ++j) {
        Cost c = cost[row * m + j];
        ++scanned;

        if (!std::isfinite(c)) continue;  // skip forbidden edges

        // Profit for this column (higher is better for the bidder)
        // profit = p[j] - cost[i,j] = -(cost[i,j] - p[j])
        // We want to maximize this (minimize reduced cost)
        Price profit = col_price[j] - c;

        if (profit > result.best_profit) {
            // New best - old best becomes second
            result.second_profit = result.best_profit;
            result.second_best_col = result.best_col;
            result.best_profit = profit;
            result.best_col = j;
        } else if (profit > result.second_profit) {
            // New second best
            result.second_profit = profit;
            result.second_best_col = j;
        }
    }

    if (edges_scanned) *edges_scanned = scanned;
    return result;
}

// Sparse version: only scan edges in adjacency list
// adj[row] = list of (column, cost) pairs
inline BestColumns find_best_columns_sparse(
    const std::vector<std::vector<std::pair<int, Cost>>>& adj,
    int row,
    const std::vector<Price>& col_price,
    int* edges_scanned = nullptr
) {
    BestColumns result;
    result.best_col = UNASSIGNED;
    result.second_best_col = UNASSIGNED;
    result.best_profit = -INF_COST;
    result.second_profit = -INF_COST;

    int scanned = 0;

    for (const auto& [j, c] : adj[row]) {
        ++scanned;
        if (!std::isfinite(c)) continue;

        Price profit = col_price[j] - c;

        if (profit > result.best_profit) {
            result.second_profit = result.best_profit;
            result.second_best_col = result.best_col;
            result.best_profit = profit;
            result.best_col = j;
        } else if (profit > result.second_profit) {
            result.second_profit = profit;
            result.second_best_col = j;
        }
    }

    if (edges_scanned) *edges_scanned = scanned;
    return result;
}

// Execute a single bid: row i bids for its best column
// Returns true if a bid was made (row had at least one feasible column)
// Updates: col_price, assignments
// Records: edges scanned for complexity tracking
//
// For MINIMIZATION auction:
// - value[j] = col_price[j] - cost[i,j] (higher is better = lower cost)
// - Bidding DECREASES col_price[j] to make this column less attractive to others
// - Bid amount = (best_value - second_best_value) + epsilon
inline bool execute_bid(
    const std::vector<Cost>& cost,
    int n, int m,
    int row,
    MatchingState& state,
    Price epsilon,
    int* edges_scanned = nullptr
) {
    BestColumns best = find_best_columns(cost, n, m, row, state.col_price, edges_scanned);

    if (best.best_col == UNASSIGNED) {
        return false;  // no feasible column for this row
    }

    int j = best.best_col;
    Price bid = best.bid_amount(epsilon);

    // Update column price: DECREASE for minimization
    // This makes the column less attractive to other bidders
    state.col_price[j] -= bid;

    // Assign row to column (this may displace another row)
    state.assign(row, j);

    return true;
}

// Execute one full auction round: all currently unassigned rows bid
// Returns number of bids made
// Theoretical bound: O(m) total edges scanned across all bids
inline int auction_round(
    const std::vector<Cost>& cost,
    int n, int m,
    MatchingState& state,
    Price epsilon,
    int* total_edges_scanned = nullptr
) {
    // Collect unassigned rows at start of round
    std::vector<int> unassigned;
    for (int i = 0; i < n; ++i) {
        if (state.row_to_col[i] == UNASSIGNED) {
            unassigned.push_back(i);
        }
    }

    int bids_made = 0;
    int edges = 0;

    for (int i : unassigned) {
        // Row might have been assigned during this round if another row
        // displaced it - but in Gauss-Seidel mode we still let it bid
        int scanned = 0;
        if (execute_bid(cost, n, m, i, state, epsilon, &scanned)) {
            ++bids_made;
        }
        edges += scanned;
    }

    if (total_edges_scanned) *total_edges_scanned = edges;
    return bids_made;
}

// Run auction until no unassigned rows or no progress
// Returns total rounds executed
inline int run_auction_phase(
    const std::vector<Cost>& cost,
    int n, int m,
    MatchingState& state,
    Price epsilon,
    int max_rounds = -1,  // -1 = unlimited
    int* total_edges_scanned = nullptr
) {
    int rounds = 0;
    int edges = 0;

    while (!state.is_complete()) {
        if (max_rounds >= 0 && rounds >= max_rounds) break;

        int scanned = 0;
        int bids = auction_round(cost, n, m, state, epsilon, &scanned);
        edges += scanned;
        ++rounds;

        if (bids == 0) break;  // no progress possible
    }

    if (total_edges_scanned) *total_edges_scanned = edges;
    return rounds;
}

// =============================================================================
// Reverse Auction: Columns bid for rows
// =============================================================================

// For reverse auction, we need best/second-best row for a column
struct BestRows {
    int best_row;
    int second_best_row;
    Price best_profit;     // Highest profit (for this column)
    Price second_profit;

    Price bid_amount(Price epsilon) const {
        if (second_best_row == UNASSIGNED) {
            return epsilon;  // Only one feasible row
        }
        return (best_profit - second_profit) + epsilon;
    }
};

// Find the best and second-best rows for column j (reverse auction)
// For minimization: profit = row_price[i] - cost[i,j]
// (symmetric to forward auction but on the row dimension)
inline BestRows find_best_rows(
    const std::vector<Cost>& cost,  // row-major n x m
    int n, int m,
    int col,
    const std::vector<Price>& row_price,
    int* edges_scanned = nullptr
) {
    BestRows result;
    result.best_row = UNASSIGNED;
    result.second_best_row = UNASSIGNED;
    result.best_profit = -INF_COST;
    result.second_profit = -INF_COST;

    int scanned = 0;

    for (int i = 0; i < n; ++i) {
        Cost c = cost[i * m + col];
        ++scanned;

        if (!std::isfinite(c)) continue;

        // Profit for this row (higher is better for the column bidder)
        // profit = row_price[i] - cost[i,j]
        Price profit = row_price[i] - c;

        if (profit > result.best_profit) {
            result.second_profit = result.best_profit;
            result.second_best_row = result.best_row;
            result.best_profit = profit;
            result.best_row = i;
        } else if (profit > result.second_profit) {
            result.second_profit = profit;
            result.second_best_row = i;
        }
    }

    if (edges_scanned) *edges_scanned = scanned;
    return result;
}

// Execute a single reverse bid: column j bids for its best row
// For MINIMIZATION reverse auction:
// - profit[i] = row_price[i] - cost[i,j] (higher is better)
// - Bidding INCREASES row_price[i] to make this row less attractive to others
inline bool execute_reverse_bid(
    const std::vector<Cost>& cost,
    int n, int m,
    int col,
    MatchingState& state,
    Price epsilon,
    int* edges_scanned = nullptr
) {
    BestRows best = find_best_rows(cost, n, m, col, state.row_price, edges_scanned);

    if (best.best_row == UNASSIGNED) {
        return false;  // no feasible row for this column
    }

    int i = best.best_row;
    Price bid = best.bid_amount(epsilon);

    // Update row price: INCREASE for minimization (reverse)
    // This makes the row less attractive to other column bidders
    state.row_price[i] += bid;

    // Check if row i was already assigned to another column
    int old_col = state.row_to_col[i];
    if (old_col != UNASSIGNED && old_col != col) {
        // Unassign from old column
        state.col_to_row[old_col] = UNASSIGNED;
    }

    // Assign column to row
    state.row_to_col[i] = col;
    state.col_to_row[col] = i;

    return true;
}

// Execute one full reverse auction round: all unassigned columns bid
inline int reverse_auction_round(
    const std::vector<Cost>& cost,
    int n, int m,
    MatchingState& state,
    Price epsilon,
    int* total_edges_scanned = nullptr
) {
    // Collect unassigned columns at start of round
    std::vector<int> unassigned;
    for (int j = 0; j < m; ++j) {
        if (state.col_to_row[j] == UNASSIGNED) {
            unassigned.push_back(j);
        }
    }

    int bids_made = 0;
    int edges = 0;

    for (int j : unassigned) {
        int scanned = 0;
        if (execute_reverse_bid(cost, n, m, j, state, epsilon, &scanned)) {
            ++bids_made;
        }
        edges += scanned;
    }

    if (total_edges_scanned) *total_edges_scanned = edges;
    return bids_made;
}

// =============================================================================
// Bidirectional Auction: Alternates between forward and reverse
// =============================================================================

// Run bidirectional auction until no progress or completion
// Returns total rounds executed
inline int run_bidirectional_auction(
    const std::vector<Cost>& cost,
    int n, int m,
    MatchingState& state,
    Price epsilon,
    int* total_edges_scanned = nullptr
) {
    int edges = 0;
    int min_size = std::min(n, m);

    // Safety limit to prevent infinite loops
    int max_total_rounds = n * n;
    int rounds = 0;
    int no_progress = 0;

    while (state.matching_size() < min_size && rounds < max_total_rounds) {
        // Forward auction: rows bid for columns
        int scanned = 0;
        int forward_bids = auction_round(cost, n, m, state, epsilon, &scanned);
        edges += scanned;
        rounds++;

        if (state.matching_size() >= min_size) break;

        // Reverse auction: columns bid for rows
        scanned = 0;
        int reverse_bids = reverse_auction_round(cost, n, m, state, epsilon, &scanned);
        edges += scanned;
        rounds++;

        if (state.matching_size() >= min_size) break;

        // Check for no progress - at this ε, neither direction can proceed
        if (forward_bids == 0 && reverse_bids == 0) {
            break;  // Done at this ε - need to scale down
        }
    }

    if (total_edges_scanned) *total_edges_scanned = edges;
    return rounds;
}

} // namespace orlin

#endif // ORLIN_BIDDING_H
