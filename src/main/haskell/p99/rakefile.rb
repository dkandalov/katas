task default: ['compile']

task :compile do
  Dir.chdir('src') do
    sh 'ghc -c P00.hs P00Test.hs P00_.hs P00_Test.hs'
  end
end
