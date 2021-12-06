using LinearAlgebra

function parse_line_segments(path::String)::Array{Int}
    line_segments = Array{Int}(undef, 1, 2)
    open(path) do file
        lines = readlines(file)
        for line in lines
            coordinates = split(line, " -> ")
            for coordinate in coordinates
                x = parse(Int, split(coordinate, ",")[1])
                y = parse(Int, split(coordinate, ",")[2])
                line_segments = vcat(line_segments, [x y])
            end
        end
    end
    return line_segments[2:end, :]
end

function draw_map(line_segments)
    min, max = minimum(line_segments), maximum(line_segments)
    field = zeros(Int, max-min+1, max-min+1)
    for i in 1:2:size(line_segments)[1]
        x1, y1 = line_segments[i, 1] - min + 1, line_segments[i, 2] - min + 1
        x2, y2 = line_segments[i+1, 1] - min + 1, line_segments[i+1, 2] - min + 1
        draw_line(field, (x1, y1), (x2, y2)) 
    end
    return field
end

function draw_line(field, p1, p2)
    x1, y1 = p1
    x2, y2 = p2
    x_step = x1 < x2 ? 1 : -1
    y_step = y1 < y2 ? 1 : -1
    if x1 == x2
        field[y1:y_step:y2, x1] = field[y1:y_step:y2, x1] .+ 1
    elseif y1 == y2
        field[y1, x1:x_step:x2] = field[y1, x1:x_step:x2] .+ 1
    elseif abs(x1-x2) == abs(y1-y2)
        for (x, y) in zip(x1:x_step:x2, y1:y_step:y2)
            field[y, x] += 1
        end
    end
end

function find_dangerous_areas(map)
    overlaps = map .> 1
    return sum(overlaps)
end

function main()
    path = "./inputs/puzzle.txt"
    segments = parse_line_segments(path)
    map = draw_map(segments)
    display(map)
    overlaps = find_dangerous_areas(map)
    println(overlaps)
end

main()