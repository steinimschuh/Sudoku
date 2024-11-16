#! /usr/bin/env ruby -w

sodoku_1 = [[0, 6, 0, 9, 2, 8, 0, 0, 0],
        [7, 8, 1, 0, 0, 0, 0, 0, 0],
        [9, 0, 0, 0, 0, 0, 8, 3, 6],
        [0, 4, 2, 8, 1, 9, 0, 0, 0],
        [0, 7, 5, 0, 0, 0, 1, 8, 0],
        [0, 0, 0, 5, 7, 6, 2, 4, 0],
        [4, 5, 8, 0, 0, 0, 0, 0, 1],
        [0, 0, 0, 0, 0, 0, 4, 9, 2],
        [0, 0, 0, 1, 4, 3, 0, 7, 0]]

sodoku_2 = [[0, 0, 0, 5, 0, 1, 9, 0, 3],
        [0, 5, 4, 0, 0, 9, 0, 0, 2],
        [0, 9, 0, 0, 2, 3, 0, 0, 6],
        [0, 0, 7, 2, 4, 0, 3, 0, 0],
        [4, 0, 5, 0, 0, 0, 7, 0, 9],
        [0, 0, 8, 0, 9, 5, 1, 0, 0],
        [5, 0, 0, 8, 6, 0, 0, 3, 0],
        [1, 0, 0, 9, 0, 0, 4, 5, 0],
        [7, 0, 3, 1, 0, 4, 0, 0, 0]]

sodoku_3 = [[0, 0, 0, 9, 0, 0, 0, 0, 0],
        [0, 6, 0, 0, 5, 1, 0, 0, 0],
        [1, 0, 4, 0, 0, 0, 3, 0, 2],
        [4, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 2, 0, 0, 0, 0, 0, 9],
        [0, 0, 0, 3, 0, 0, 7, 2, 6],
        [0, 0, 6, 0, 1, 8, 0, 0, 0],
        [0, 0, 0, 0, 0, 3, 2, 0, 0],
        [8, 7, 1, 5, 0, 9, 4, 0, 0]]

sodoku_4 = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 5, 1, 9, 3],
        [0, 0, 6, 4, 0, 3, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 1],
        [0, 0, 0, 0, 0, 0, 0, 8, 0],
        [0, 1, 0, 8, 0, 9, 0, 7, 6],
        [4, 9, 0, 1, 0, 0, 0, 3, 0],
        [0, 0, 2, 3, 0, 0, 0, 0, 8],
        [8, 0, 0, 6, 0, 4, 0, 5, 9]]

sodoku_5 = [[0, 0, 0, 0, 2, 0, 0, 7, 0],
        [4, 0, 0, 0, 1, 8, 0, 0, 5],
        [0, 7, 3, 0, 0, 0, 0, 0, 9],
        [0, 0, 0, 0, 0, 0, 7, 0, 0],
        [0, 0, 0, 0, 8, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 9, 0, 0, 3],
        [0, 9, 0, 0, 0, 5, 0, 0, 0],
        [8, 0, 0, 6, 0, 1, 0, 3, 0],
        [0, 0, 2, 0, 0, 0, 4, 0, 0]]

sodoku_6 = [[0, 0, 0, 0, 0, 0, 9, 0, 7],
       [0, 0, 0, 4, 2, 0, 1, 8, 0],
       [0, 0, 0, 7, 0, 5, 0, 2, 6],
       [1, 0, 0, 9, 0, 4, 0, 0, 0],
       [0, 5, 0, 0, 0, 0, 0, 4, 0],
       [0, 0, 0, 5, 0, 7, 0, 0, 9],
       [9, 2, 0, 1, 0, 8, 0, 0, 0],
       [0, 3, 4, 0, 5, 9, 0, 0, 0],
       [5, 0, 7, 0, 0, 0, 0, 0, 0]]

class Sodoku
        attr_reader :row, :col, :box

        def Sodoku.flipflop (x, y)
                h = y.divmod(3)
                v = x.divmod(3)
                return [v[0]*3+h[0], v[1]*3+h[1]]
        end

        def initialize (a)
                @row = Array.new(9) { |i| Array.new(9) { |j| [a[i][j]] }}
                @col = Array.new(9) { |i| Array.new(9) { |j| @row[j][i] } }
                @box = Array.new(9) { |i| Array.new(9) { |j| x, y = Sodoku.flipflop(i, j); @row[x][y] }}
                @flat = Array.new(81) { |i| x, y = i.divmod(9); @row[x][y] }
        end

        def [] (x) 
                @flat[x][0] 
        end
        
        def []= (x, a)
                @flat[x][0] = a
        end

        def slots ()
                (0..80).to_a.select { |i| @flat[i][0] == 0 }
        end

        def choices (a)
                a.collect do |e|
                        x, y = e.divmod(9)
                        b, _ = Sodoku.flipflop(x, y)
                        [e, ((1..9).to_a.map { |e| [e] } - @row[x] - @col[y] - @box[b]).map { |e| e[0] }]
                end.sort { |a,b| a[1].length <=> b[1].length }  
        end

        def reset (x)
                @flat[x][0] = 0
        end

        def to_a ()
                @row.map do |r| r.map { |c| c[0]} end
        end
end

def solve(a)
        sodoku = Sodoku.new(a)
        queue = []

        loop do
                # Test if there are still slots to be filled. If not, you are done.
                slots = sodoku.slots
                if slots == []
                        break
                end

                i, values = sodoku.choices(slots)[0]
                if values != []
                        # All changes so far did not lead to a contradiction.
                        # Hence: trafers down.
                        sodoku[i] = values.shift
                        queue.push([i, values])
                else
                        # A change let do a contradiction. Now: try a different or undo.
                        loop do
                                i, values = queue.pop
                                if values != []
                                        # There are still values to be tried.
                                        sodoku[i] = values.shift
                                        queue.push([i, values])
                                        break
                                else
                                        # Tried all possible values. Need to undone and go back up.
                                        sodoku.reset(i)
                                end
                        end
                end
        end

        sodoku.to_a
end

p solve(sodoku_6)
