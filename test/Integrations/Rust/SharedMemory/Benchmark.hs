{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.Benchmark where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup

spec :: Spec
spec =
    describe "Benchmark" $ do
    describe "labyrinth" $ do
        it "with conditionals" $
            -- the function below is closely resembling the naive sequential implementation: 
            -- https://github.com/Feliix42/ohua-rust-benchmarks/blob/master/labyrinth/src/bin/simple_sequential.rs#L122-L133
            -- upon writing the snippet below I realized that we might need to alter the code a bit for the "retry" semantics introduced with the batching transformation
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use funs::*;

                fn route_paths(mut maze: Maze, to_map: Vec<(Point, Point)>) -> Maze {
                    for pair in to_map {
                        let mapped = find_path(pair, &maze.grid);
                        if mapped.is_some() {
                            let path = mapped.unwrap();
                            // this is the implementation in the original, but something like this is also possible I reckon:
                            // maze.update(path);
                            update_maze(&mut maze, path);
                        } else {
                            // Same here: The specification could most certainly altered to exclude the direct struct member access with something like:
                            // maze.add_unmappable(pair);
                            maze.unmappable_paths.push(pair);
                        }
                    }

                    maze
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test(i: i32) -> i32 {
                            TODO
                        }
                    |]
                compiled `shouldBe` expected)
        it "no conditionals - amorphous" $
          -- this is the version that we promise in the paper
          -- FIXME mutability is missing until the according issue was addressed. see sertel/ohuac-integrations#4
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use benchs::*;
                use std::*;

                // FIXME: Add 2 muts here once supported
                fn fill(maze: Maze, pairs: Vec<Option<(Point, Point)>>, its: u32) -> Maze {
                    let rs = Vec::default();
                    // let rs = UnmappedPaths::default();
                    let m2 = maze.clone(); // the type check for state threads in Ohua forces me to put this here. this is good!
                    let mro = Arc::new(m2);
                    for pair in pairs {
                        // FIXME This type check seems not be implemented yet.
                        //       The test `var multi fail` also does not show the desired result: an error message!
                        let path = find_path(mro.clone(), pair);
                        let r = maze.update(path);
                        rs.push(r);
                    }
                    // rs.evict_mapped();
                    let rs1 = filter_mapped(rs);
                    let rs2 = rs1.clone();
                    let (new_its, not_done) = calculate_done(rs1, its);
                    // let new_its_left = decrement(its_left);
                    // let new_its_left1 = new_its_left.clone();
                    // // let not_done = rs.calculate_done1(new_its_left);
                    // let not_done = calculate_done(rs1, new_its_left);
                    if not_done { fill(maze, rs2, new_its) }
                    else { maze }
                }

                pub fn run(dimensions: Point, pairs: Vec<Option<(Point, Point)>>, max_it:u32) -> Maze {
                    let maze = Maze::init(dimensions);
                    fill(maze, pairs, max_it)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test(i: i32) -> i32 {
                            TODO
                        }
                    |]
                compiled `shouldBe` expected)
        it "no conditionals" $
          -- this is the version that we promise in the paper
          -- FIXME mutability is missing until the according issue was addressed. see sertel/ohuac-integrations#4
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use benchs::*;
                use std::*;

                fn fill(mut maze: Maze, pairs: Vec<(Point, Point)>, its_left: u32) -> Maze {
                    let mut rs = Vec::default();
                    let mro = maze.clone(); // the type check for state threads in Ohua forces me to put this here. this is good!
                    for pair in pairs {
                        // FIXME This type check seems not be implemented yet.
                        //       The test `var multi fail` also does not show the desired result: an error message!
                        let path = find_path(mro.clone(), pair);
                        let r = maze.update(path);
                        rs.push(r);
                    }
                    // FIXME destructuring currently does not work due to ...
                    // FIXME the algorithm that gathers the defined variables in the frontend does not seem to understand destructured patterns.
                    // let (rs1,not_done,new_its_left) = get_unmapped(rs,its_left);
                    let rs1 = filter_mapped(rs);
                    let rs2 = rs1.clone();
                    let new_its_left = decrement(its_left);
                    // FIXME: This *could* be a copy, but we don't understand that yet
                    let new_its_left1 = new_its_left.clone();
                    let not_done = calculate_done(rs1, new_its_left);
                    if not_done { fill(maze, rs2, new_its_left1) }
                    else { maze }
                }

                pub fn run(salt: i32, pairs: Vec<(Point, Point)>, max_it:u32) -> Maze {
                    let maze = Maze::init(salt);
                    fill(maze, pairs, max_it)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test(i: i32) -> i32 {
                            TODO
                        }
                    |]
                compiled `shouldBe` expected)
    it "blackscholes" $
        (showCode "Compiled: " =<< compileCode  [sourceFile|
            use benchs::*;
            use std::*;

            // NOTE(feliix42): Kind of a hack here: we're pre-partitioning the work list
            pub fn calculate(options: Vec<Vec<OptionData>>) -> Vec<f32> {
                // TODO: this loop is not correct -> the items must be collected in a vec separately
                let results = Vec::new();
                // FIXME(feliix42): can be removed with the closure of https://github.com/sertel/ohuac/issues/29
                let ops = id(options);

                for op in ops {
                    let i = batch_calculate_black_scholes(op);
                    results.push(i);
                }

                unpack(results)
            }
            |]) >>=
        (\compiled -> do
            expected <- showCode "Expected:"
                [sourceFile|
                    use funs::*;

                    fn test(i: i32) -> i32 {
                        TODO
                    }
                |]
            compiled `shouldBe` expected)
    it "kmeans" $
        (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
            use benchs::*;
            use std::*;

            fn run(values: Vec<Vec<Value>>, centroids: Arc<Vec<Centroid>>, threshold: f32, iterations: u32) -> u32 {
                let new_values = Vec::default();

                for v in values {
                    let i = reassign_values(v, centroids.clone()); // -> (Value, f32 or u32)
                    new_values.push(i);
                }

                // now calculate the new centroids and the delta
                let (vals, delta) = evaluate_results(new_values);

                let cont = should_continue(delta, threshold.clone(), iterations.clone());
                let (new_vals, new_centroids) = create_centroids(vals, centroids);
                let inc_iter = inc(iterations);

                if cont { run(new_vals, new_centroids, threshold, inc_iter) }
                else { inc_iter }
            }

            pub fn calculate(values: Vec<Vec<Value>>, centroids: Arc<Vec<Centroid>>, threshold: f32, iterations: u32) -> u32 {
                run(values, centroids, threshold, iterations)
            }
            |]) >>=
        (\compiled -> do
            expected <- showCode "Expected:"
                [sourceFile|
                    use funs::*;

                    fn test(i: i32) -> i32 {
                        TODO
                    }
                |]
            compiled `shouldBe` expected)
--    it "canneal" $
--        (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
--            use benchs::*;
--            use std::*;
--
--            // FIXME collapse netlist and internal_state into one state
--            fn run(mut netlist: Netlist, worklist: Vec<Result<MoveDecision, (usize, usize)>>, temperature: f64, mut internal_state: InternalState) -> Netlist {
--                let mut rs = Vec::new(); // the new worklist
--
--                let new_temp: f64 = reduce_temp(temperature);
--
--                let n2: Netlist = netlist.clone();
--                let nro: Arc<Netlist> = Arc::new(n2);
--                for item in worklist {
--                    let switch_info: (MoveDecision, (usize, usize)) = process_move(item, nro.clone(), new_temp.clone());
--                    // updates the netlist by performing the switch, returning an error when there's a collision
--                    rs.push(switch_info);
--                }
--
--                // let new_netlist = extract(nro);
--                let foo: Vec<(MoveDecision, (usize, usize))> = netlist.update(rs);
--                // netlist.clear_changes();
--                let rs2: Vec<(MoveDecision, (usize, usize))> = foo.clone();
--                let mut remaining_work: Vec<(MoveDecision, (usize, usize))> = filter_work(foo);
--                let length: usize = remaining_work.len();
--
--                // FIXME: I need this to show the compiler that new_temp is *not* the amorphous variable
--                let (new_temp2, new_temp3) = dup(new_temp);
--
--                // get new work if necessary
--                // get whether to continue
--                let (new_work, keep_going) = internal_state.assess_updates(rs2, length);
--                // add new work items
--                remaining_work.exp(new_work);
--
--                if keep_going {
--                    run(netlist, remaining_work, new_temp3, internal_state)
--                } else {
--                    netlist
--                }
--            }
--
--            pub fn annealer(netlist: Netlist, elements: usize, temperature: f64, max_steps: Option<i32>, swaps_per_temp: usize) -> Netlist {
--                let st = InternalState::initialize(elements, max_steps, swaps_per_temp);
--
--                let worklist = st.generate_worklist();
--                run(netlist, worklist, temperature, st)
--            }
--            |]) >>=
--        (\compiled -> do
--            expected <- showCode "Expected:"
--                [sourceFile|
--                    use funs::*;
--
--                    fn test(i: i32) -> i32 {
--                        TODO
--                    }
--                |]
--            compiled `shouldBe` expected)
    it "canneal2" $
        (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
            use benchs::*;
            use std::*;

            fn run(mut state: NetlistAndInternal, worklist: Vec<Result<MoveDecision, (usize, usize)>>, temperature: f64) -> NetlistAndInternal {
                let mut rs = Vec::new(); // the new worklist
                let new_temp: f64 = reduce_temp(temperature);
                let n2: NetlistAndInternal = state.clone();
                let nro: Arc<NetlistAndInternal> = Arc::new(n2);
                for item in worklist {
                    let switch_info: (MoveDecision, (usize, usize)) = process_move(item, nro.clone(), new_temp.clone());
                    // updates the netlist by performing the switch, returning an error when there's a collision
                    rs.push(switch_info);
                }

                let remaining_work: Vec<(MoveDecision, (usize, usize))> = state.update(rs);
                let keep_going = state.get_keep_going();

                if keep_going {
                    run(state, remaining_work, new_temp)
                } else {
                    state
                }
            }

            pub fn annealer(netlist: NetlistAndInternal, worklist:Vec<Result<MoveDecision, (usize, usize)>>, temperature: f64) -> NetlistAndInternal {
                run(netlist, worklist, temperature)
            }
            |]) >>=
        (\compiled -> do
            expected <- showCode "Expected:"
                [sourceFile|
                    use funs::*;

                    fn test(i: i32) -> i32 {
                        TODO
                    }
                |]
            compiled `shouldBe` expected)
    it "ptdr" $
        (showCode "Compiled: " =<< compileCodeWithDebug [sourceFile|
            // use crate::{
            //     losprobprofile::{NoLimitProbProfile, SegmentsHistoryProbProfile},
            //     probprofile::quartiles::Quartiles,
            // };
            // use crate::{
            //     //delay::delay_profile,
            //     route::Route,
            //     simulation::{Simulation, SingleSimulation},
            // };
            // use crate::helpers;
            // use chrono::{DateTime, Duration, Utc};
            // use datatimebox::timerange::TimeRange;
            // use std::sync::Arc;
            use std::*;
            use ptdr::*;

            fn delay_profile(
                s: Arc<Route<String>>,
                departure_time: DateTime<Utc>,
                p: Arc<SegmentsHistoryProbProfile<String, Quartiles>>,
                samples: usize,
            ) -> Vec<Duration> {
                let no_limit = Arc::new(NoLimitProbProfile::new4());
                let route: Arc<Route<String>> = id(s);
                let prob_profile: Arc<SegmentsHistoryProbProfile<String, Quartiles>> = id(p);
                let free_flow_duration = route.drive(departure_time, no_limit);

                // delay for each sample -> parallelism would normally be gained by mapping in parallel (using
                // rayon)
                let mut res = Vec::new();
                let r = sample_range(samples);
                for _ in r {
                    let duration = route.drive(departure_time, prob_profile.clone());
                    // let delta: Duration = duration - free_flow_duration;
                    let delta: Duration = sub(duration, free_flow_duration);

                    res.push(delta);
                }

                res
            }

            pub fn run(
                rp: String,
                departure_time: DateTime<Utc>,
                time_span: Duration,
                step_size: Duration,
                s: Arc<Route<String>>,
                p: Arc<SegmentsHistoryProbProfile<String, Quartiles>>,
                sam: usize,
            ) -> Simulation {
                // TODO: Why can't I use `route` or `prob_profile` etc directly?
                let route: Arc<Route<String>> = id(s);
                let route_path: String = id(rp);
                let prob_profile: Arc<SegmentsHistoryProbProfile<String, Quartiles>> = id(p);
                let samples: usize = id(sam);
                let mut simulations = Simulation::new2(route_path.as_str());
                let tr = TimeRange::new1(departure_time, time_span);
                for dt in tr.step_by(step_size) {
                    let mut sim = SingleSimulation::new3(dt, samples.clone());
                    let delays = delay_profile(route.clone(), dt, prob_profile.clone(), samples);
                    sim.set_samples(delays);
                    simulations.add(sim);
                }

                simulations
            }
            |]) >>=
        (\compiled -> do
            expected <- showCode "Expected:"
                [sourceFile|
                    use funs::*;

                    fn test(i: i32) -> i32 {
                        TODO
                    }
                |]
            compiled `shouldBe` expected)
