pipeline {
  agent any

  stages {
    stage('Build') {
      steps {
        echo 'Building..'
        sh('stack build --setup-ghc')
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
