using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RInstaller.Models
{
    public class SqlInstance
    {
        public string Id { get; set; }

        public bool Enabled { get; set; }

        public bool Bound { get; set; }

        public bool Checked { get; set; }
    }
}
