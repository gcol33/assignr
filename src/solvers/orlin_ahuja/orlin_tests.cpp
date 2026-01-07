// src/solvers/orlin_ahuja/orlin_tests.cpp
// Exported test functions for verifying Orlin-Ahuja components
// These allow R to test correctness and measure empirical complexity

#include <Rcpp.h>
#include <chrono>
#include <random>
#include "orlin_types.h"
#include "orlin_prices.h"
#include "orlin_bidding.h"
#include "orlin_ssp.h"
#include "orlin_scaling.h"

using namespace Rcpp;
using namespace orlin;

// =============================================================================
// Component 1 Tests: Price/Potential Management
// =============================================================================

// Test: Compute single reduced cost
double oa_test_reduced_cost_impl(NumericMatrix cost_r, int i, int j,
                                 NumericVector row_price_r, NumericVector col_price_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    std::vector<Price> row_price(row_price_r.begin(), row_price_r.end());
    std::vector<Price> col_price(col_price_r.begin(), col_price_r.end());

    // Convert from 1-based R indexing
    return reduced_cost(cost, n, m, i - 1, j - 1, row_price, col_price);
}

// Test: Compute all reduced costs with timing
List oa_test_all_reduced_costs_impl(NumericMatrix cost_r,
                                    NumericVector row_price_r,
                                    NumericVector col_price_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    std::vector<Price> row_price(row_price_r.begin(), row_price_r.end());
    std::vector<Price> col_price(col_price_r.begin(), col_price_r.end());

    auto start = std::chrono::high_resolution_clock::now();
    auto rc = compute_all_reduced_costs(cost, n, m, row_price, col_price);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // Convert to R matrix
    NumericMatrix rc_r(n, m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            rc_r(ii, jj) = rc[ii * m + jj];
        }
    }

    return List::create(
        Named("reduced_costs") = rc_r,
        Named("time_us") = time_us,
        Named("n") = n,
        Named("m") = m,
        Named("nm") = n * m
    );
}

// Test: Check epsilon-CS
bool oa_test_epsilon_cs_impl(NumericMatrix cost_r,
                             IntegerVector row_to_col_r,  // 1-based, 0 = unassigned
                             NumericVector row_price_r,
                             NumericVector col_price_r,
                             double epsilon) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    MatchingState state(n, m);
    for (int i = 0; i < n; ++i) {
        state.row_price[i] = row_price_r[i];
        int j = row_to_col_r[i];
        if (j > 0) {  // 1-based to 0-based
            state.row_to_col[i] = j - 1;
            state.col_to_row[j - 1] = i;
        }
    }
    for (int j = 0; j < m; ++j) {
        state.col_price[j] = col_price_r[j];
    }

    return check_epsilon_cs(cost, n, m, state, epsilon);
}

// =============================================================================
// Component 2 Tests: Best/Second-Best Column Finding
// =============================================================================

// Test: Find best columns for a row with timing
List oa_test_find_best_columns_impl(NumericMatrix cost_r, int row,
                                    NumericVector col_price_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    std::vector<Price> col_price(col_price_r.begin(), col_price_r.end());

    int edges_scanned = 0;
    auto start = std::chrono::high_resolution_clock::now();
    BestColumns result = find_best_columns(cost, n, m, row - 1, col_price, &edges_scanned);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    return List::create(
        Named("best_col") = result.best_col + 1,  // back to 1-based
        Named("second_best_col") = result.second_best_col == UNASSIGNED ? 0 : result.second_best_col + 1,
        Named("best_profit") = result.best_profit,
        Named("second_profit") = result.second_profit,
        Named("edges_scanned") = edges_scanned,
        Named("time_us") = time_us
    );
}

// =============================================================================
// Component 3 Tests: Single Auction Round
// =============================================================================

// Test: Execute single bid
List oa_test_single_bid_impl(NumericMatrix cost_r, int row,
                             IntegerVector row_to_col_r,
                             NumericVector row_price_r,
                             NumericVector col_price_r,
                             double epsilon) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    MatchingState state(n, m);
    for (int i = 0; i < n; ++i) {
        state.row_price[i] = row_price_r[i];
        int j = row_to_col_r[i];
        if (j > 0) {
            state.row_to_col[i] = j - 1;
            state.col_to_row[j - 1] = i;
        }
    }
    for (int j = 0; j < m; ++j) {
        state.col_price[j] = col_price_r[j];
    }

    int edges_scanned = 0;
    auto start = std::chrono::high_resolution_clock::now();
    bool success = execute_bid(cost, n, m, row - 1, state, epsilon, &edges_scanned);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // Convert back to R format
    IntegerVector new_row_to_col(n);
    for (int i = 0; i < n; ++i) {
        new_row_to_col[i] = state.row_to_col[i] == UNASSIGNED ? 0 : state.row_to_col[i] + 1;
    }

    NumericVector new_col_price(m);
    for (int j = 0; j < m; ++j) {
        new_col_price[j] = state.col_price[j];
    }

    return List::create(
        Named("success") = success,
        Named("row_to_col") = new_row_to_col,
        Named("col_price") = new_col_price,
        Named("edges_scanned") = edges_scanned,
        Named("time_us") = time_us
    );
}

// Test: Full auction round with timing
List oa_test_auction_round_impl(NumericMatrix cost_r,
                           IntegerVector row_to_col_r,
                           NumericVector row_price_r,
                           NumericVector col_price_r,
                           double epsilon) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    MatchingState state(n, m);
    for (int i = 0; i < n; ++i) {
        state.row_price[i] = row_price_r[i];
        int j = row_to_col_r[i];
        if (j > 0) {
            state.row_to_col[i] = j - 1;
            state.col_to_row[j - 1] = i;
        }
    }
    for (int j = 0; j < m; ++j) {
        state.col_price[j] = col_price_r[j];
    }

    int edges_scanned = 0;
    auto start = std::chrono::high_resolution_clock::now();
    int bids_made = auction_round(cost, n, m, state, epsilon, &edges_scanned);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // Convert back
    IntegerVector new_row_to_col(n);
    for (int i = 0; i < n; ++i) {
        new_row_to_col[i] = state.row_to_col[i] == UNASSIGNED ? 0 : state.row_to_col[i] + 1;
    }

    NumericVector new_col_price(m);
    for (int j = 0; j < m; ++j) {
        new_col_price[j] = state.col_price[j];
    }

    return List::create(
        Named("bids_made") = bids_made,
        Named("row_to_col") = new_row_to_col,
        Named("col_price") = new_col_price,
        Named("matching_size") = state.matching_size(),
        Named("edges_scanned") = edges_scanned,
        Named("time_us") = time_us
    );
}

// =============================================================================
// Complexity Measurement: Run multiple sizes and return timing data
// =============================================================================

DataFrame oa_complexity_reduced_costs_impl(IntegerVector sizes, int reps = 5) {
    std::vector<int> n_vec, m_vec;
    std::vector<double> time_vec;
    std::mt19937 rng(42);
    std::uniform_real_distribution<double> dist(1.0, 100.0);

    for (int size : sizes) {
        int n = size;
        int m = size;

        // Generate random cost matrix
        std::vector<Cost> cost(n * m);
        for (int i = 0; i < n * m; ++i) {
            cost[i] = dist(rng);
        }

        std::vector<Price> row_price(n, 0.0);
        std::vector<Price> col_price(m, 0.0);

        // Warm up
        compute_all_reduced_costs(cost, n, m, row_price, col_price);

        // Timed runs
        for (int rep = 0; rep < reps; ++rep) {
            auto start = std::chrono::high_resolution_clock::now();
            compute_all_reduced_costs(cost, n, m, row_price, col_price);
            auto end = std::chrono::high_resolution_clock::now();

            double time_us = std::chrono::duration<double, std::micro>(end - start).count();
            n_vec.push_back(n);
            m_vec.push_back(m);
            time_vec.push_back(time_us);
        }
    }

    return DataFrame::create(
        Named("n") = n_vec,
        Named("m") = m_vec,
        Named("nm") = IntegerVector(n_vec.begin(), n_vec.end()) *
                      IntegerVector(m_vec.begin(), m_vec.end()),
        Named("time_us") = time_vec
    );
}

DataFrame oa_complexity_auction_round_impl(IntegerVector sizes, int reps = 3) {
    std::vector<int> n_vec, m_vec, edges_vec;
    std::vector<double> time_vec;
    std::mt19937 rng(42);
    std::uniform_real_distribution<double> dist(1.0, 100.0);

    for (int size : sizes) {
        int n = size;
        int m = size;

        // Generate random cost matrix
        std::vector<Cost> cost(n * m);
        for (int i = 0; i < n * m; ++i) {
            cost[i] = dist(rng);
        }

        for (int rep = 0; rep < reps; ++rep) {
            // Fresh state - all unassigned
            MatchingState state(n, m);

            int edges_scanned = 0;
            auto start = std::chrono::high_resolution_clock::now();
            auction_round(cost, n, m, state, 1.0, &edges_scanned);
            auto end = std::chrono::high_resolution_clock::now();

            double time_us = std::chrono::duration<double, std::micro>(end - start).count();
            n_vec.push_back(n);
            m_vec.push_back(m);
            edges_vec.push_back(edges_scanned);
            time_vec.push_back(time_us);
        }
    }

    return DataFrame::create(
        Named("n") = n_vec,
        Named("m") = m_vec,
        Named("edges_scanned") = edges_vec,
        Named("time_us") = time_vec
    );
}

// =============================================================================
// Component 4 Tests: Dijkstra/SSP
// =============================================================================

// Test: Single Dijkstra shortest path search
// Note: Internal Dijkstra function is now integrated into ssp_augment_once.
// This test function is kept for API compatibility but just runs ssp_augment_once.
List oa_test_dijkstra_impl(NumericMatrix cost_r,
                           IntegerVector row_to_col_r,
                           NumericVector row_price_r,
                           NumericVector col_price_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    MatchingState state(n, m);
    for (int i = 0; i < n; ++i) {
        state.row_price[i] = row_price_r[i];
        int j = row_to_col_r[i];
        if (j > 0) {
            state.row_to_col[i] = j - 1;
            state.col_to_row[j - 1] = i;
        }
    }
    for (int j = 0; j < m; ++j) {
        state.col_price[j] = col_price_r[j];
    }

    // Find an unassigned row to use as source
    int source_row = find_unassigned_row(state);
    if (source_row == UNASSIGNED) {
        // All rows are assigned, return empty result
        return List::create(
            Named("found") = false,
            Named("target_col") = 0,
            Named("distances") = NumericVector(m, R_PosInf),
            Named("predecessors") = IntegerVector(m, 0),
            Named("edges_scanned") = 0,
            Named("heap_ops") = 0,
            Named("time_us") = 0.0
        );
    }

    int edges_scanned = 0;
    auto start = std::chrono::high_resolution_clock::now();
    bool found = ssp_augment_once(cost, n, m, state, &edges_scanned);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // For compatibility, return stub distances and predecessors
    NumericVector dist_r(m, R_PosInf);
    IntegerVector pred_r(m, 0);

    // We can't get the target_col from the new interface since it's internal
    // Just return what we know
    return List::create(
        Named("found") = found,
        Named("target_col") = 0,  // Not available in new interface
        Named("distances") = dist_r,
        Named("predecessors") = pred_r,
        Named("edges_scanned") = edges_scanned,
        Named("heap_operations") = 0,
        Named("time_us") = time_us
    );
}

// Test: Full SSP phase
List oa_test_ssp_phase_impl(NumericMatrix cost_r,
                            IntegerVector row_to_col_r,
                            NumericVector row_price_r,
                            NumericVector col_price_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    MatchingState state(n, m);
    for (int i = 0; i < n; ++i) {
        state.row_price[i] = row_price_r[i];
        int j = row_to_col_r[i];
        if (j > 0) {
            state.row_to_col[i] = j - 1;
            state.col_to_row[j - 1] = i;
        }
    }
    for (int j = 0; j < m; ++j) {
        state.col_price[j] = col_price_r[j];
    }

    auto start = std::chrono::high_resolution_clock::now();
    SSPStats stats = run_ssp_phase(cost, n, m, state);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // Convert matching to R format
    IntegerVector new_row_to_col(n);
    for (int i = 0; i < n; ++i) {
        new_row_to_col[i] = state.row_to_col[i] == UNASSIGNED ? 0 : state.row_to_col[i] + 1;
    }

    return List::create(
        Named("row_to_col") = new_row_to_col,
        Named("matching_size") = state.matching_size(),
        Named("augmentations") = stats.augmentations,
        Named("total_edges_scanned") = stats.total_edges_scanned,
        Named("total_heap_ops") = stats.total_heap_ops,
        Named("total_path_length") = stats.total_path_length,
        Named("time_us") = time_us
    );
}

// =============================================================================
// Component 5 Tests: Epsilon Scaling
// =============================================================================

// Test: Count number of scales for given parameters
List oa_test_scale_count_impl(double max_cost, int n, double alpha) {
    ScalingParams params;
    params.alpha = alpha;
    params.initial_eps = max_cost;
    params.final_eps = 1.0 / (n + 1);

    int scales = 0;
    double epsilon = params.initial_eps;
    std::vector<double> epsilons;

    while (epsilon >= params.final_eps && scales < 1000) {
        epsilons.push_back(epsilon);
        epsilon /= params.alpha;
        scales++;
    }

    double theoretical_scales = std::log(max_cost * n) / std::log(alpha);

    return List::create(
        Named("n_scales") = scales,
        Named("theoretical_scales") = theoretical_scales,
        Named("initial_eps") = params.initial_eps,
        Named("final_eps") = params.final_eps,
        Named("epsilons") = epsilons
    );
}

// =============================================================================
// Component 6 Tests: Full Orlin-Ahuja Solver
// =============================================================================

// Test: Full solve with detailed statistics
List oa_solve_impl(NumericMatrix cost_r, double alpha = 5.0,
                   int auction_rounds = 10) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    ScalingParams params;
    params.alpha = alpha;
    params.auction_rounds_per_scale = auction_rounds;

    auto start = std::chrono::high_resolution_clock::now();
    OrlinResult result = solve_orlin_ahuja(cost, n, m, params);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // Convert matching to R format (1-based)
    IntegerVector row_to_col_r(n);
    for (int i = 0; i < n; ++i) {
        row_to_col_r[i] = result.row_to_col[i] == UNASSIGNED ? 0 : result.row_to_col[i] + 1;
    }

    // Extract per-scale statistics
    int n_scales = result.scales.size();
    IntegerVector scale_nums(n_scales);
    NumericVector scale_eps(n_scales);
    IntegerVector scale_auction_rounds(n_scales);
    IntegerVector scale_auction_bids(n_scales);
    IntegerVector scale_ssp_augs(n_scales);
    IntegerVector scale_edges(n_scales);

    for (int s = 0; s < n_scales; ++s) {
        scale_nums[s] = result.scales[s].scale_number;
        scale_eps[s] = result.scales[s].epsilon;
        scale_auction_rounds[s] = result.scales[s].auction_rounds;
        scale_auction_bids[s] = result.scales[s].auction_bids;
        scale_ssp_augs[s] = result.scales[s].ssp_augmentations;
        scale_edges[s] = result.scales[s].edges_scanned;
    }

    DataFrame scale_stats = DataFrame::create(
        Named("scale") = scale_nums,
        Named("epsilon") = scale_eps,
        Named("auction_rounds") = scale_auction_rounds,
        Named("auction_bids") = scale_auction_bids,
        Named("ssp_augmentations") = scale_ssp_augs,
        Named("edges_scanned") = scale_edges
    );

    return List::create(
        Named("row_to_col") = row_to_col_r,
        Named("total_cost") = result.total_cost,
        Named("optimal") = result.optimal,
        Named("n_scales") = result.total_scales,
        Named("total_augmentations") = result.total_augmentations,
        Named("scale_stats") = scale_stats,
        Named("time_us") = time_us
    );
}

// Complexity test: measure augmentations per scale (CRITICAL for √n verification)
DataFrame oa_complexity_augmentations_per_scale_impl(IntegerVector sizes, int reps = 3) {
    std::vector<int> n_vec;
    std::vector<double> sqrt_n_vec;
    std::vector<double> max_augs_per_scale_vec;
    std::vector<double> avg_augs_per_scale_vec;
    std::vector<int> n_scales_vec;
    std::vector<double> time_vec;

    std::mt19937 rng(42);
    std::uniform_real_distribution<double> dist(1.0, 100.0);

    for (int size : sizes) {
        int n = size;
        int m = size;

        for (int rep = 0; rep < reps; ++rep) {
            // Generate random cost matrix
            std::vector<Cost> cost(n * m);
            for (int i = 0; i < n * m; ++i) {
                cost[i] = dist(rng);
            }

            ScalingParams params;
            params.alpha = 5.0;
            params.auction_rounds_per_scale = 5;

            auto start = std::chrono::high_resolution_clock::now();
            OrlinResult result = solve_orlin_ahuja(cost, n, m, params);
            auto end = std::chrono::high_resolution_clock::now();

            double time_us = std::chrono::duration<double, std::micro>(end - start).count();

            // Find max and average augmentations per scale
            int max_augs = 0;
            int total_augs = 0;
            for (const auto& s : result.scales) {
                int augs = s.auction_bids + s.ssp_augmentations;
                if (augs > max_augs) max_augs = augs;
                total_augs += augs;
            }
            double avg_augs = result.scales.empty() ? 0 :
                             static_cast<double>(total_augs) / result.scales.size();

            n_vec.push_back(n);
            sqrt_n_vec.push_back(std::sqrt(static_cast<double>(n)));
            max_augs_per_scale_vec.push_back(max_augs);
            avg_augs_per_scale_vec.push_back(avg_augs);
            n_scales_vec.push_back(result.total_scales);
            time_vec.push_back(time_us);
        }
    }

    return DataFrame::create(
        Named("n") = n_vec,
        Named("sqrt_n") = sqrt_n_vec,
        Named("max_augs_per_scale") = max_augs_per_scale_vec,
        Named("avg_augs_per_scale") = avg_augs_per_scale_vec,
        Named("n_scales") = n_scales_vec,
        Named("time_us") = time_vec
    );
}
