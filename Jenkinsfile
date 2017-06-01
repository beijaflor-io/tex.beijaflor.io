pipeline {
  agent any

  stages {
    stage('Build') {
      steps {
        echo 'Building..'
        withDockerContainer 'fpco/stack-build' {
          shell 'stack build'
        }
      }
    }

    stage('Test') {
      steps {
        echo 'Testing..'
      }
    }

    stage('Deploy') {
      steps {
        echo 'Deploying....'
      }
    }
  }
}
