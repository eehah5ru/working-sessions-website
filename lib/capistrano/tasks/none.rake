# # -*- mode: ruby -*-

# namespace :none do
#   # desc 'Check if local "repo" exists & is a directory'
#   # task :check do
#   #   unless File.directory?(repo_url)
#   #     raise "none.rake: #{repo_url} must be a directory!"
#   #   end
#   # end

#   # desc 'Upload files'
#   # task :create_release do
#   #   on roles :all do
#   #     info 'Uploading files for release'
#   #     upload! repo_url, release_path, recursive: true
#   #   end
#   # end
#   task :check do
#     unless File.directory?(repo_url)
#       raise "none.rake: #{repo_url} must be a directory!"
#     end
#   end

#   task :create_release do
#     on release_roles(:all) do |_host|
#       execute :mkdir, '-p', release_path
#     end
#   end

#   task :set_current_revision do
#     on release_roles(:web) do |_host|
#       execute :ln, '-s', release_path, current_path
#     end
#   end
# end
