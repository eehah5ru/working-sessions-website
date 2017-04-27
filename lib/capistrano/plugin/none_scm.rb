require "capistrano/scm/plugin"

# By convention, Capistrano plugins are placed in the
# Capistrano namespace. This is completely optional.
module Capistrano
  class NoneScmPlugin < ::Capistrano::SCM::Plugin
    def set_defaults
      # Define any variables needed to configure the plugin.
      # set_if_empty :myvar, "my-default-value"
    end

    def define_tasks
      # The namespace can be whatever you want, but its best
      # to choose a name that matches your plugin name.
      namespace :none_scm do
        task :check do
          unless File.directory?(repo_url)
            raise "none_scm.rake: #{repo_url} must be a directory!"
          end
        end

        task :create_release do
          on release_roles :all do
            execute :mkdir, "-p", release_path
            upload! repo_url, release_path, recursive: true
          end
        end

        task :set_current_revision do
          on release_roles(:all) do |_host|
            execute :rm, current_path
            execute :ln, '-s', release_path, current_path
          end
        end

      end
    end

    def register_hooks
      # Tell Capistrano to run the custom create_release task
      # during deploy.
      after "deploy:new_release_path", "none_scm:create_release"
      before "deploy:set_current_revision", "none_scm:set_current_revision"
    end
  end
end
