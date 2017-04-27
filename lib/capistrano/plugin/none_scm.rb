require "capistrano/scm/plugin"

# By convention, Capistrano plugins are placed in the
# Capistrano namespace. This is completely optional.
module Capistrano
  class NoneScmPlugin < ::Capistrano::SCM::Plugin
    def set_defaults
      # Define any variables needed to configure the plugin.
      # set_if_empty :myvar, "my-default-value"
      set_if_empty :archive_name, "archive.tar.gz"
      set_if_empty :none_scm_verbose, ""
    end

    def define_tasks
      # The namespace can be whatever you want, but its best
      # to choose a name that matches your plugin name.
      namespace :none_scm do
        #
        # make tarbal file locally
        #
        #
        # check repo_url
        #
        task :check do
          unless File.directory?(repo_url)
            raise "none_scm.rake: #{repo_url} must be a directory!"
          end

          if fetch(:archive_name).nil?
            raise "none_scm.rake :archive_name must be a valid filename"
          end
        end

        #
        # make tarball
        # deploy it on the server
        # extract tarball
        # rm tarball from the server
        #
        task :create_release => :make_archive do |t|
          on release_roles :all do

            # STDERR.puts "NONE_SCM.create_release: #{fetch(:archive_name)}"


            tarball = fetch(:archive_name)

            execute :mkdir, "-p", release_path

            # Create a temporary file on the server
            tmp_file = capture("mktemp")

            # Upload the archive, extract it and finally remove the tmp_file
            upload!(tarball, tmp_file)
            execute :tar, "-xzf", tmp_file, "-C", release_path
            execute :rm, tmp_file
            # upload! repo_url, release_path, recursive: true
          end
        end

        task :make_archive do
          files = Rake::FileList[File.join(repo_url, "*")].exclude(fetch :archive_name).map { |f| File.basename(f) }
          cmd = ["tar -C #{repo_url} -c#{fetch :none_scm_verbose}zf  #{fetch :archive_name}", *files]
          sh cmd.join(' ')
        end

        # task :create_release_deps do
        #   desc "Archive files to #{fetch :archive_name}"
        #   files = Rake::FileList[repo_url].exclude(fetch :archive_name)

        #   file fetch(:archive_name) => files do |t|
        #     cmd = ["tar -c#{fetch :none_scm_verbose}zf #{t.name}", *t.prerequisites]
        #     sh cmd.join(' ')
        #   end

        #   task :create_release => fetch(:archive_name)
        # end


        task :set_current_revision do
          set :current_revision, `git log --pretty=format:'%h | %ai | %d %s' -1`
          # on release_roles(:all) do |_host|
          #   # execute :rm, current_path
          #   execute :ln, '-s', release_path, current_path
          # end
        end

        task :clean do |t|
          # Delete the local archive
          File.delete fetch(:archive_name) if File.exists? fetch(:archive_name)
        end

        # task :add_revision_file do
        #   on roles(:all) do
        #     rev = %x[git log --pretty=format:'%H' -1]

        #     within release_path do
        #       execute "echo '#{rev}' > REVISION"
        #     end
        #   end
        # end
      end
    end

    def register_hooks
      #
      # infrasructure
      #
      # before "none_scm:create_release", "none_scm:create_release_deps"

      # Tell Capistrano to run the custom create_release task
      # during deploy.
      after "deploy:check", "none_scm:check"
      after "deploy:new_release_path", "none_scm:create_release"
      before "deploy:set_current_revision", "none_scm:set_current_revision"
      # after 'deploy:updating', 'none_scm:add_revision_file'
      after 'deploy:finished', 'none_scm:clean'

      # error handling
      after 'deploy:failed', 'none_scm:clean'
    end
  end
end
