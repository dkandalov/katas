require "rake"
require "rake/clean"

IMAGE_FILES = FileList['images/**/*.png']
p IMAGE_FILES
THUMB_FILES = IMAGE_FILES.pathmap("%{^images,thumbs}d/%n-thumb%x")
p THUMB_FILES

CLEAN.include(THUMB_FILES, "thumbs")
CLOBBER.include("final.png")

#file "thumbs/gem-thumb.png" => ["images/gem.png"] do
#  sh 'convert -thumbnail 32x32 images/gem.png thumbs/gem-thumb.png'
#end

directory 'thumbs'

THUMB_FILES.zip(IMAGE_FILES).each do |target, source|
  containing_dir = target.pathmap("%d")
  directory containing_dir
  file target => [containing_dir, source] do
    #sh "convert -thumbnail 32x32 #{source} #{target}"
    sh "cp #{source} #{target}"
  end
end

file 'final.png' => THUMB_FILES do
  #sh "convert #{THUMB_FILES} -append final.png"
  sh "touch thumbs/final.png"
end

task :convert => "final.png"
task :default => :convert

