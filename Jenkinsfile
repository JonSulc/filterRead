pipeline {
  agent { label 'hpc'}

  environment {
    PATH = "/opt/R/4.5.1/bin:${env.PATH}"
    R_LIBS_USER = "~/rcp_storage/common/Users/sulc/jenkins-r-libs"
  }

  stages {
    stage('Setup') {
      steps {
        sh '''
          # Verify R is accessible
          Rscript --version

          # Verify tabix is accessible
          which tabix

          mkdir -p ${R_LIBS_USER}

          git config user.email "jonathan.sulc@epfl.ch"
          git config user.name "Jenkins CI"
        '''
      }
    }

    stage('Install Dependencies') {
      steps {
        sh '''
          Rscript -e "
            dir.create(Sys.getenv('R_LIBS_USER'), recursive = TRUE, showWarnings = FALSE)
            .libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths()))
            if (!require('devtools', quietly = TRUE)) {
              install.packages('devtools', repos = 'https://cloud.r-project.org', lib = Sys.getenv('R_LIBS_USER'))
            }
            devtools::install_deps(dependencies = TRUE, upgrade = 'never', lib = Sys.getenv('R_LIBS_USER'))
        "
        '''
      }
    }

    stage('Document') {
      steps {
        sh '''
          Rscript -e "
            .libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths()))
            devtools::document()
          "
        '''
      }
    }

    stage('Commit Documentation') {
      steps {
        script {
          def changes = sh(
            script: '''git status --porcelain man/ \
NAMESPACE DESCRIPTION''',
            returnStdout: true
          ).trim()
          if (changes) {
            withCredentials([
              usernamePassword(
                credentialsId: 'github---pat',
                usernameVariable: 'GIT_USERNAME',
                passwordVariable: 'GIT_PASSWORD'
              )
            ]) {
              sh '''
                git add man/ NAMESPACE DESCRIPTION
                git commit -m \
"docs: update documentation [ci skip]"
                # Strip origin/ prefix from branch name if present
                BRANCH=${GIT_BRANCH#origin/}
                # Use PAT for authenticated push
                git push https://${GIT_USERNAME}:${GIT_PASSWORD}\
@github.com/${GIT_URL#*github.com/} HEAD:${BRANCH}
              '''
            }
            echo 'Documentation updated and pushed.'
          } else {
            echo 'No documentation changes to commit.'
          }
        }
      }
    }

    stage('Check') {
      steps {
        sh '''
          Rscript -e "
            .libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths()))
            devtools::check(error_on = 'error')
          "
        '''
      }
    }
  }

  post {
    always {
      cleanWs(cleanWhenNotBuilt: false,
              deleteDirs: true,
              disableDeferredWipeout: true,
              notFailBuild: true)
    }
    success {
      echo 'Pipeline completed successfully!'
    }
    failure {
      echo 'Pipeline failed. Check the logs for details.'
    }
  }
}