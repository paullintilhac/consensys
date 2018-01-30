#include <string>
#include <fstream>
#include <iostream>	
#include <sstream>

using namespace std;


int main(int argc, char* argv[]){
	ifstream infile("c:/users/paul/downloads/txs_sample3.csv");
		cout<<"badbit: "<<infile.bad()<<
		", failbit: "<<infile.fail()<<
		", eof: "<<infile.eof()<<
		", good: "<<infile.good()<<endl;
}

