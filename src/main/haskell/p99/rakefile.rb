task default: ['compile']

task :compile do
  Dir.chdir('src') do
    # files = %w{P00 P00Test P00_ P00_Test}
    # files.each do |file|
    #   sh "rm #{file}" if File.exist?(file)
    #   sh "rm #{file}.o" if File.exist?(file + '.o')
    #   sh "rm #{file}.hi" if File.exist?(file + '.hi')
    # end
    sh 'ghc -c P00.hs P00Test.hs P00_.hs P00_Test.hs P50.hs P70.hs playground.hs'
    sh 'ghc P00Test.hs'
    sh 'ghc P00_Test.hs'
    sh 'ghc P50_Test.hs'
    sh 'ghc P70_Test.hs'
    sh 'ghc playground.hs'
  end
end
