{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Benchmark where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils

spec :: Spec
spec =
    describe "Benchmark" $
    describe "labyrinth" $ do
        it "with conditionals" $
            -- the function below is closely resembling the naive sequential implementation: 
            -- https://github.com/Feliix42/ohua-rust-benchmarks/blob/master/labyrinth/src/bin/simple_sequential.rs#L122-L133
            -- upon writing the snippet below I realized that we might need to alter the code a bit for the "retry" semantics introduced with the batching transformation
            (showCode "Compiled: " =<< compileCode [sourceFile|
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
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use benchs::*;
                use std::*;

                // FIXME: Add 2 muts here once supported
                fn fill(maze: Maze, pairs: Vec<Option<(Point, Point)>>, its_left: u32) -> Maze {
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
                    let (new_its_left, not_done) = calculate_done(rs1, its_left);
                    // let new_its_left = decrement(its_left);
                    // let new_its_left1 = new_its_left.clone();
                    // // let not_done = rs.calculate_done1(new_its_left);
                    // let not_done = calculate_done(rs1, new_its_left);
                    if not_done { fill(maze, rs2, new_its_left) }
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
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
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
