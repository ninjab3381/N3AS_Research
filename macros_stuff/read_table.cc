#include <string>
#include <iostream>
#include <filesystem>
#include <vector>
#include <fstream>
#include <sstream>


using namespace std;
namespace fs = std::filesystem;
using std::filesystem::current_path;



int main()
{
    string fname;
    std::string path = "./S_v_E_files_mod";
    std::cout << "Current working directory: " << current_path() << std::endl;

    std::vector<std::string> list_of_files{};


    for (const auto & entry : fs::directory_iterator(path))
        list_of_files.push_back(entry.path());

    vector<vector<string>> content;
    vector<string> row;
    string line, word;

    for (string & fname : list_of_files) {

        std::cout << "reading: " << fname << std::endl;
        //fname = "./S_v_E_files_mod/Marcucci.csv";
        fstream file (fname, ios::in);
        if(file.is_open())
        {
            getline(file, line);
            while(getline(file, line))
            {
                row.clear();

                std::istringstream str( line );

                //stringstream str(line);

                while(getline(str, word, ','))
                    row.push_back(word);
                content.push_back(row);
            }
        }
        else
            cout<<"Could not open the file\n";

    }

    for(int i=0;i<content.size();i++)
    {
    for(int j=0;j<2;j++)
    {
  
        std::stringstream geek(content[i][j]);
  
        // The object has the value 12345 and stream
        // it to the integer x
        float x = 0;
        geek >> x;
        
    cout<<x<<" ";
    }
    cout<<"\n";
    }




    return 0;
}
