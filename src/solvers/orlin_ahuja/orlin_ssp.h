// src/solvers/orlin_ahuja/orlin_ssp.h
// Component 4: SSP (Successive Shortest Path) for Orlin-Ahuja
// Uses Hungarian-style potential updates for correctness
// Complexity: O(n^2) per shortest path with simple arrays

#ifndef ORLIN_SSP_H
#define ORLIN_SSP_H

#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include "orlin_types.h"

namespace orlin {

// =============================================================================
// SSP Statistics
// =============================================================================

struct SSPStats {
    int augmentations;
    int total_edges_scanned;
    int total_heap_ops;
    int total_path_length;
};

// Find any unassigned row
inline int find_unassigned_row(const MatchingState& state) {
    for (int i = 0; i < state.n; ++i) {
        if (state.row_to_col[i] == UNASSIGNED) {
            return i;
        }
    }
    return UNASSIGNED;
}

// =============================================================================
// Hungarian-style SSP: Find shortest augmenting path and update
// =============================================================================
//
// This is the standard shortest augmenting path algorithm for assignment:
// 1. Dijkstra from unassigned row to find shortest path to unassigned column
// 2. Track which rows are "in the tree" to update their potentials
// 3. Update both row and column potentials (Johnson's reweighting)
// 4. Augment along the path
//
// Key invariant: reduced_cost[i,j] = cost[i,j] - u[i] - v[j] >= 0 for all edges
// After augmentation: matched edges have reduced_cost = 0

inline bool ssp_augment_once(
    const std::vector<Cost>& cost,
    int n, int m,
    MatchingState& state,
    int* edges_scanned_out = nullptr
) {
    // Find an unassigned row to start from
    int source_row = find_unassigned_row(state);
    if (source_row == UNASSIGNED) {
        return false;
    }

    // Dijkstra arrays
    std::vector<Price> dist(m, INF_COST);      // Distance to each column
    std::vector<int> pred_col(m, UNASSIGNED);  // Predecessor column (or -1 if from source row)
    std::vector<bool> col_done(m, false);      // Column finalized
    std::vector<bool> row_in_tree(n, false);   // Rows that are part of the shortest path tree
    int edges_scanned = 0;

    // Source row is in the tree
    row_in_tree[source_row] = true;

    // Initialize: distances from source_row to all columns
    // Reduced cost = cost[src,j] - u[src] - v[j]
    for (int j = 0; j < m; ++j) {
        Cost c = cost[source_row * m + j];
        if (c < INF_COST) {
            dist[j] = c - state.row_price[source_row] - state.col_price[j];
            pred_col[j] = -1;  // Came from source row
            edges_scanned++;
        }
    }

    // Dijkstra main loop
    int target_col = UNASSIGNED;
    Price target_dist = INF_COST;

    while (true) {
        // Find minimum unfinalized column
        int best_col = UNASSIGNED;
        Price best_dist = INF_COST;
        for (int j = 0; j < m; ++j) {
            if (!col_done[j] && dist[j] < best_dist) {
                best_dist = dist[j];
                best_col = j;
            }
        }

        if (best_col == UNASSIGNED || best_dist >= INF_COST) {
            break;  // No more reachable columns
        }

        col_done[best_col] = true;

        // Check if unassigned -> found shortest augmenting path
        if (state.col_to_row[best_col] == UNASSIGNED) {
            target_col = best_col;
            target_dist = best_dist;
            break;
        }

        // Column is matched - add its row to the tree and relax edges
        int matched_row = state.col_to_row[best_col];
        row_in_tree[matched_row] = true;

        for (int j = 0; j < m; ++j) {
            if (col_done[j]) continue;

            Cost c = cost[matched_row * m + j];
            if (c >= INF_COST) continue;

            edges_scanned++;

            // Reduced cost from matched_row to j
            Price rc = c - state.row_price[matched_row] - state.col_price[j];
            Price new_dist = best_dist + rc;

            if (new_dist < dist[j]) {
                dist[j] = new_dist;
                pred_col[j] = best_col;
            }
        }
    }

    if (edges_scanned_out) {
        *edges_scanned_out += edges_scanned;
    }

    if (target_col == UNASSIGNED) {
        return false;  // No augmenting path found
    }

    // Update potentials using Johnson's reweighting
    // For rows in the tree: u'[i] = u[i] + (delta - dist[matched_col[i]])
    //   (where matched_col is the column that added row i to the tree)
    // For reached columns: v'[j] = v[j] + (delta - dist[j])
    //
    // BUT: The above formulation is for when we track distances to rows.
    // We're tracking distances to columns only, so we need a different update.
    //
    // Correct formulation for column-distance-only Dijkstra:
    // After finding shortest path from source s to target t with dist[t] = delta:
    // - For all columns j reached (col_done[j] == true):
    //   v'[j] = v[j] - (delta - dist[j])
    // - For all rows i in the tree:
    //   u'[i] = u[i] + delta  (if i == source)
    //   u'[i] = u[i] + delta - dist[matched_col[i]] (if i != source, came via matched_col[i])

    Price delta = target_dist;

    // Update column potentials for all reached columns
    // This makes reduced costs of path edges become 0
    for (int j = 0; j < m; ++j) {
        if (col_done[j]) {
            state.col_price[j] -= (delta - dist[j]);
        }
    }

    // Update row potentials for all rows in the tree
    // Source row: u += delta
    state.row_price[source_row] += delta;

    // Other rows in tree: u[i] += delta - dist[col_that_added_i]
    for (int i = 0; i < n; ++i) {
        if (row_in_tree[i] && i != source_row) {
            int matched_col = state.row_to_col[i];
            if (matched_col != UNASSIGNED && col_done[matched_col]) {
                state.row_price[i] += (delta - dist[matched_col]);
            }
        }
    }

    // Augment along the path
    int j = target_col;

    while (j != UNASSIGNED) {
        int prev_col = pred_col[j];
        int reaching_row;

        if (prev_col == -1) {
            reaching_row = source_row;
        } else {
            reaching_row = state.col_to_row[prev_col];
        }

        // Unassign reaching_row from its current column (if any)
        if (state.row_to_col[reaching_row] != UNASSIGNED) {
            int old_col = state.row_to_col[reaching_row];
            state.col_to_row[old_col] = UNASSIGNED;
        }

        // Assign reaching_row to j
        state.row_to_col[reaching_row] = j;
        state.col_to_row[j] = reaching_row;

        j = prev_col;
    }

    return true;
}

// =============================================================================
// Full SSP Phase: Find all augmenting paths until matching is complete
// =============================================================================

// Run SSP until matching is complete (or no path found)
// Returns statistics about the search
inline SSPStats run_ssp_phase(
    const std::vector<Cost>& cost,
    int n, int m,
    MatchingState& state
) {
    SSPStats stats = {0, 0, 0, 0};
    int min_size = std::min(n, m);
    int max_iterations = n * m;  // Safety limit

    while (state.matching_size() < min_size && stats.augmentations < max_iterations) {
        bool found = ssp_augment_once(cost, n, m, state, &stats.total_edges_scanned);

        if (!found) {
            break;  // No augmenting path exists
        }

        stats.augmentations++;
    }

    return stats;
}

}  // namespace orlin

#endif  // ORLIN_SSP_H
