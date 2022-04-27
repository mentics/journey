module RunTests
using Test

function runTestsAll()::Nothing
    @testset "All Tests" begin
        for (root, dirs, files) in walkdir("modules")
            occursin("ignore", root) && continue
            for file in filter(x->endswith(x, "Test.jl"), files)
                _, fn = splitdir(file)
                testName, _ = splitext(fn)
                # @info "Running test" testName
                eval(Meta.parse("using $(testName)"))
                eval(Meta.parse("$(testName).runTests()"))
            end
        end
    end
    return
end

end